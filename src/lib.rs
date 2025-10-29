
const TURN_OFF_HASH_BASED_LAZY_RENDER: usize = 0;

use std::{alloc::{alloc, dealloc, Layout}, hash::Hasher, hint::spin_loop, ptr::{copy_nonoverlapping, slice_from_raw_parts}, rc::Rc, sync::{atomic::{AtomicU32, Ordering}, Barrier}, time::{Duration, Instant}};
use twox_hash::xxhash3_64;
use winit::dpi::Size;

const RENDER_TILE_SHIFT: usize = 7;
const RENDER_TILE_SIZE: usize = 1 << RENDER_TILE_SHIFT;
const RENDER_TILE_INTRA_MASK: usize = RENDER_TILE_SIZE.wrapping_sub(1);

struct ThreadContext {
    wake_up_gate: AtomicU32,
    workers_that_have_passed_the_wake_up_gate: AtomicU32,
    wake_up_barrier: Barrier,
    is_last_time: bool,
    thread_count: u32,
    begin_work_gate: AtomicU32,
    workers_at_job_site: AtomicU32,
    work_unit_count: u32,
    work_unit_take: AtomicU32,
    work_unit_complete: AtomicU32,
    work_user_pointer: usize,
    work_user_function: WideWorkFn,
}
type WideWorkFn = fn(thread_id: usize, work_id: usize, work_count: usize, user_pointer: usize);

fn worker_thread_loop(thread_id: usize, p_thread_context: usize) {
    unsafe {
        let p_thread_context = p_thread_context as *mut ThreadContext;
        let thread_count = (*p_thread_context).thread_count;
        loop {
            (*p_thread_context).wake_up_barrier.wait();
            while (*p_thread_context).wake_up_gate.load(Ordering::Acquire) == 0 { spin_loop(); }
            (*p_thread_context).workers_that_have_passed_the_wake_up_gate.fetch_add(1, Ordering::Relaxed);

            loop {
                while (*p_thread_context).begin_work_gate.load(Ordering::Relaxed) == 0 { spin_loop(); }
                (*p_thread_context).workers_at_job_site.fetch_add(1, Ordering::Relaxed);
                if (*p_thread_context).begin_work_gate.load(Ordering::Acquire) == 0 {
                    // abort, exit job site and await orders again.
                    (*p_thread_context).workers_at_job_site.fetch_sub(1, Ordering::Release);
                    continue;
                }

                let is_last_time = (*p_thread_context).is_last_time;
                let work_count = (*p_thread_context).work_unit_count as usize;
                let work_user_pointer = (*p_thread_context).work_user_pointer;
                let work_user_function = (*p_thread_context).work_user_function;
                loop {
                    let work_id = (*p_thread_context).work_unit_take.fetch_add(1, Ordering::Relaxed) as usize;
                    if work_id < work_count {
                        work_user_function(thread_id, work_id, work_count, work_user_pointer);
                        
                        (*p_thread_context).work_unit_complete.fetch_add(1, Ordering::Release);
                    } else {
                        break;
                    }
                }
                (*p_thread_context).workers_at_job_site.fetch_sub(1, Ordering::Relaxed);
                if is_last_time { break; }
            }
        }
    }
}

fn dennis_parallel_for(p_thread_context: *mut ThreadContext, is_last_time: bool, work_count: usize, work_user_pointer: usize, work_user_function: WideWorkFn) {
    unsafe {
        let thread_count = (*p_thread_context).thread_count;
        
        // Begin work gate should be closed.
        // We wait for the job site to be clear.
        while (*p_thread_context).workers_at_job_site.load(Ordering::Relaxed) != 0 { spin_loop(); }
        
        (*p_thread_context).is_last_time = is_last_time;
        (*p_thread_context).work_unit_count = work_count as u32;
        (*p_thread_context).work_unit_take.store(0, Ordering::Relaxed);
        (*p_thread_context).work_unit_complete.store(0, Ordering::Relaxed);
        (*p_thread_context).work_user_pointer = work_user_pointer;
        (*p_thread_context).work_user_function = work_user_function;
        
        // Open the gate and let workers descend.
        (*p_thread_context).begin_work_gate.store(1, Ordering::Release);
        // We could increment job site workers here but we are the only one who reads it so we will not.
        
        loop {
            let work_id = (*p_thread_context).work_unit_take.fetch_add(1, Ordering::Relaxed) as usize;
            if work_id < work_count {
                work_user_function(0, work_id, work_count, work_user_pointer);
                
                (*p_thread_context).work_unit_complete.fetch_add(1, Ordering::Release);
            } else {
                break;
            }
        }
        // All work is already done or in progress by someone so we can close the gate for new workers.
        // But, if it is the last time we need to leave it open in order to let workers see that and go to sleep.
        if is_last_time == false
        { (*p_thread_context).begin_work_gate.store(0, Ordering::Relaxed); }
    
        while (*p_thread_context).work_unit_complete.load(Ordering::Acquire) < work_count as u32 { spin_loop(); }
    }
}

struct EndOfFrameBlitCtx {
    render_target_0: *mut u8,
    display_buffer: *mut u8,
    row_count: usize,
    render_target_stride: usize,
    display_buffer_stride: usize,
}

fn end_of_frame_row_stride_copy(thread_id: usize, work_id: usize, work_count: usize, user_pointer: usize) {
    unsafe {
        let p_thread_context = user_pointer as *mut EndOfFrameBlitCtx;
        let render_target_0 = (*p_thread_context).render_target_0;
        let display_buffer = (*p_thread_context).display_buffer;
        let row_count = (*p_thread_context).row_count;
        let render_target_stride = (*p_thread_context).render_target_stride;
        let display_buffer_stride = (*p_thread_context).display_buffer_stride;

        let mut local_row_count = row_count / work_count;
        let start_row = work_id * local_row_count;
        if work_id == work_count - 1 {
            local_row_count = row_count - start_row;
        }

        for row in start_row..start_row+local_row_count {
            let pixel_src = render_target_0.byte_add(row*render_target_stride*4) as *mut u8;
            let pixel_dst = display_buffer.byte_add(row*display_buffer_stride*4) as *mut u8;
            copy_nonoverlapping(pixel_src, pixel_dst, display_buffer_stride*4);
        }
    }
}

enum DrawCommand {
    ColoredRectangle {
        x: u32,
        x2: u32,
        y: u32,
        y2: u32,
        color: u32,
    },
}

#[derive(Clone, Copy)]
struct ExecuteCommandBufferOnTilesCtx {
    render_target_0: *mut u8,
    render_target_stride: usize,
    window_width: usize,
    window_height: usize,
    saved_tile_hashes: *mut u64,
    draw_commands: *const DrawCommand,
    draw_command_count: usize,
}

fn execute_command_buffer_on_tiles(thread_id: usize, work_id: usize, work_count: usize, user_pointer: usize) {
    unsafe {
        let ctx = *(user_pointer as *const ExecuteCommandBufferOnTilesCtx);
        let pixel_row_shift = ctx.render_target_stride.trailing_zeros() as usize;
        let intra_row_mask = ctx.render_target_stride.wrapping_sub(1);
        debug_assert!(work_count.count_ones() == 1);
        let tile_row_shift = work_count.trailing_zeros() / 2;
        let tile_x = work_id & (1usize << tile_row_shift).wrapping_sub(1);
        let tile_y = work_id >> tile_row_shift;

        let tile_pixel_x = (tile_x << RENDER_TILE_SHIFT) as u32;
        let tile_pixel_x2 = ((tile_x+1) << RENDER_TILE_SHIFT) as u32;
        let tile_pixel_y = (tile_y << RENDER_TILE_SHIFT) as u32;
        let tile_pixel_y2 = ((tile_y+1) << RENDER_TILE_SHIFT) as u32;
        if tile_pixel_x as usize >= ctx.window_width { return; }
        if tile_pixel_y as usize >= ctx.window_height { return; }
        
        let mut got_hash = 0u64;
        for should_draw in 0..2 {
            let should_draw = should_draw == 1;
            if should_draw {
                let saved = ctx.saved_tile_hashes.byte_add(8*work_id);
                if got_hash == *saved && TURN_OFF_HASH_BASED_LAZY_RENDER == 0 {
                    return;
                } else {
                    *saved = got_hash;
                }
            }
            let mut hasher = xxhash3_64::Hasher::new();
            for cmd_i in 0..ctx.draw_command_count {
                match *ctx.draw_commands.byte_add(size_of::<DrawCommand>()*cmd_i) {
                    DrawCommand::ColoredRectangle { mut x, mut x2, mut y, mut y2, color } => {
                        x = x.max(tile_pixel_x);
                        x2 = x2.min(tile_pixel_x2);
                        y = y.max(tile_pixel_y);
                        y2 = y2.min(tile_pixel_y2);
                        if x >= x2 || y >= y2 { continue; }
                        hasher.write_u64(0x854893982097);
                        hasher.write_u32(x);
                        hasher.write_u32(x2);
                        hasher.write_u32(y);
                        hasher.write_u32(y2);
                        hasher.write_u32(color);
                        if should_draw == false { continue; }
                        let mut row_pixels = ctx.render_target_0.byte_add(((x + (y << pixel_row_shift)) as usize) << 2);
                        for _y in y..y2 {
                            let mut cursor_pixels = row_pixels;
                            for _x in x..x2 {
                                *(cursor_pixels as *mut u32) = color;
                                cursor_pixels = cursor_pixels.byte_add(4);
                            }
                            row_pixels = row_pixels.byte_add(4 << pixel_row_shift);
                        }
                    }
                }
            }
            got_hash = hasher.finish();
        }

        //     //0x3357FF, // Strong Blue
            
        // let wide_color = u32x4::splat(0xFF5733u32.wrapping_mul(thread_id as u32));
        
        // let tile_pixels = ctx.render_target_0.byte_add(((tile_x << RENDER_TILE_SHIFT) + (tile_y << RENDER_TILE_SHIFT << pixel_row_shift)) << 2);
        // let mut row_pixels = tile_pixels;
        // for _y in 0..RENDER_TILE_SIZE {
        //     let mut cursor_pixels = row_pixels;
        //     for _x in 0..RENDER_TILE_SIZE/4 {
        //         *(cursor_pixels as *mut u32x4) = wide_color;
        //         cursor_pixels = cursor_pixels.byte_add(4*4);
        //     }
        //     row_pixels = row_pixels.byte_add(4 << pixel_row_shift);
        // }
    }
}

pub fn loop_curve(t: f64) -> (f64, f64) {
    // Tunables:
    const R: f64 = 5.0; // fixed circle radius
    const r: f64 = 3.0; // rolling circle radius
    const d: f64 = 4.0; // pen offset from the rolling circle center

    let k = (R - r) / r;      // frequency ratio (here = 2/3)
    let x = (R - r) * t.cos() + d * (k * t).cos();
    let y = (R - r) * t.sin() - d * (k * t).sin();
    (x, y)
}

fn okay_but_is_it_wayland(elwt: &winit::event_loop::ActiveEventLoop) -> bool {
    #[cfg(target_os = "linux")]
    {
        use winit::platform::wayland::ActiveEventLoopExtWayland;
        elwt.is_wayland()
    }
    #[cfg(not(target_os = "linux"))]
    {
        false
    }
}

pub fn main_thread_run_program() {
    // Create window + event loop.
    let event_loop = winit::event_loop::EventLoop::new().unwrap();
    
    let mut frame_interval_milli_hertz = 60000;
    let mut next_frame_deadline = Instant::now() + Duration::from_secs(1000) / frame_interval_milli_hertz;
    let mut prev_frame_time_us = 0u64;
    let mut last_call_to_present_instant = Instant::now();
    let mut frame_is_actually_queued_by_us = false;

    let mut s_thread_context = ThreadContext {
        wake_up_gate: AtomicU32::new(0),
        workers_that_have_passed_the_wake_up_gate: AtomicU32::new(num_cpus::get_physical() as u32 - 1),
        wake_up_barrier: Barrier::new(num_cpus::get_physical()),
        is_last_time: false,
        thread_count: num_cpus::get_physical() as u32,
        begin_work_gate: AtomicU32::new(0),
        workers_at_job_site: AtomicU32::new(0),
        work_unit_count: 0,
        work_unit_take: AtomicU32::new(0),
        work_unit_complete: AtomicU32::new(0),
        work_user_pointer: 0,
        work_user_function: |_,_,_,_| {}, 
    };
    let p_thread_context: *mut ThreadContext = &mut s_thread_context as *mut ThreadContext;

    for thread_id in 1..unsafe { (*p_thread_context).thread_count as usize } {
        let magic_int = p_thread_context as usize;
        let _ = std::thread::spawn(move || {
            println!("Started worker thread#{}...", thread_id);
            worker_thread_loop(thread_id, magic_int);
        });
    }

    let mut cached_square_width = 0;
    let mut render_target_0_alloc_layout = Layout::new::<u32>();
    let mut render_target_0 = std::ptr::null_mut();
    let mut saved_tile_hashes = Vec::new();

    let mut t = 0.0;
    let mut mouse_box_x = 0u32;
    let mut mouse_box_y = 0u32;

    let mut window: Option<Rc<winit::window::Window>> = None;
    let mut softbuffer_context: Option<softbuffer::Context<Rc<winit::window::Window>>> = None;
    let mut softbuffer_surface: Option<softbuffer::Surface<Rc<winit::window::Window>, Rc<winit::window::Window>>> = None;
    event_loop.run(move |event, elwt: &winit::event_loop::ActiveEventLoop| {
        match event {
            winit::event::Event::Resumed => { // Runs at startup and is where we have to do init.
                let twindow = Rc::new(elwt.create_window(
                    winit::window::WindowAttributes::default()
                    .with_title("winit + softbuffer")
                    .with_inner_size(Size::Physical(winit::dpi::PhysicalSize { width: 1600, height: 900 }))
                ).unwrap());
                let context = softbuffer::Context::new(twindow.clone()).unwrap();
                let surface = softbuffer::Surface::new(&context, twindow.clone()).unwrap();
                window = Some(twindow);
                softbuffer_context = Some(context);
                softbuffer_surface = Some(surface);
            },
            event => {
                if let Some(window) = window.as_mut() {
                    let softbuffer_context = softbuffer_context.as_mut().unwrap();
                    let softbuffer_surface = softbuffer_surface.as_mut().unwrap();
                    match event {
                        winit::event::Event::WindowEvent { window_id: _window_id, event } => {
                            match event {
                                winit::event::WindowEvent::CursorMoved { device_id, position } => {
                                    mouse_box_x = position.x as u32;
                                    mouse_box_y = position.y as u32;
                                },
                                winit::event::WindowEvent::RedrawRequested => {
                                    if frame_is_actually_queued_by_us || okay_but_is_it_wayland(elwt) {
                                        unsafe {
                                            // Tell workers: WAKE UP WAKE UP WAKE UP!!!
                                            while (*p_thread_context).workers_that_have_passed_the_wake_up_gate.load(Ordering::Relaxed) != (*p_thread_context).thread_count - 1 { spin_loop(); }
                                            (*p_thread_context).workers_that_have_passed_the_wake_up_gate.store(0, Ordering::Relaxed);
                                            (*p_thread_context).wake_up_gate.store(0, Ordering::Relaxed);
                                            (*p_thread_context).wake_up_barrier.wait();
                                            (*p_thread_context).is_last_time = false;
                                            (*p_thread_context).begin_work_gate.store(0, Ordering::Relaxed);
                                            (*p_thread_context).wake_up_gate.store(1, Ordering::Release);

                                            let begin_frame_instant = Instant::now();
                                            
                                            let (window_width, window_height) = {
                                                let size = window.inner_size();
                                                (size.width as usize, size.height as usize)
                                            };
                                            softbuffer_surface.resize((window_width as u32).try_into().unwrap(), (window_height as u32).try_into().unwrap()).unwrap();
                                            
                                            let mut buffer = softbuffer_surface.buffer_mut().unwrap();
                                            let final_output_blit_buffer = buffer.as_mut_ptr() as *mut u8;
                                            let window_square: usize = window_width.max(window_height);
                                            let tiles_wide = ((window_square + RENDER_TILE_SIZE - 1) / RENDER_TILE_SIZE).next_power_of_two();
                                            let draw_area_pixel_wide = tiles_wide * RENDER_TILE_SIZE;
                                            if cached_square_width != draw_area_pixel_wide {
                                                if render_target_0 != std::ptr::null_mut() {
                                                    dealloc(render_target_0, render_target_0_alloc_layout);
                                                }
                                                cached_square_width = draw_area_pixel_wide;
                                                render_target_0_alloc_layout = Layout::array::<u32>((draw_area_pixel_wide*draw_area_pixel_wide) as usize).unwrap().align_to(4096).unwrap();
                                                render_target_0 = alloc(render_target_0_alloc_layout);
                                                saved_tile_hashes = vec![0u64; tiles_wide*tiles_wide];
                                            }

                                            //////////////////////////////
                                            let dt = 1000.0 / (frame_interval_milli_hertz as f64);

                                            t += dt;
                                            let (fx, _fy) = loop_curve(t.powf(1.6));
                                            let (_fx, fy) = loop_curve(t.powf(1.7));
                                            let ix = (500.0 + fx*40.0) as u32;
                                            let iy = (300.0 + fy*30.0) as u32;

                                            let mut draw_commands = Vec::new();
                                            draw_commands.push(DrawCommand::ColoredRectangle { x: 0, x2: window_width as u32, y: 0, y2: window_height as u32, color: 0x222222 });
                                            draw_commands.push(DrawCommand::ColoredRectangle { x: ix, x2: ix + 50, y: iy, y2: iy+40, color: 0x3357FF });

                                            draw_commands.push(DrawCommand::ColoredRectangle { x: mouse_box_x, x2: mouse_box_x + 100, y: mouse_box_y, y2: mouse_box_y+50, color: 0xFF3366 });

                                            let mut ups = ExecuteCommandBufferOnTilesCtx {
                                                render_target_0,
                                                render_target_stride: draw_area_pixel_wide,
                                                window_width,
                                                window_height,
                                                saved_tile_hashes: saved_tile_hashes.as_mut_ptr(),
                                                draw_commands: draw_commands.as_ptr(),
                                                draw_command_count: draw_commands.len(),
                                            };
                                            dennis_parallel_for(p_thread_context, false, tiles_wide*tiles_wide, &ups as *const ExecuteCommandBufferOnTilesCtx as usize, execute_command_buffer_on_tiles);

                                            prev_frame_time_us = begin_frame_instant.elapsed().as_micros() as u64;
                                            
                                            let ups = EndOfFrameBlitCtx {
                                                render_target_0,
                                                display_buffer: final_output_blit_buffer,
                                                row_count: window_height,
                                                render_target_stride: draw_area_pixel_wide,
                                                display_buffer_stride: window_width,
                                            };
                                            dennis_parallel_for(p_thread_context, true, (window_height + 32 - 1) / 32, &ups as *const EndOfFrameBlitCtx as usize, end_of_frame_row_stride_copy);
                                            
                                            let frame_pace_us = last_call_to_present_instant.elapsed().as_micros() as u64;
                                            last_call_to_present_instant = Instant::now();
                                            if okay_but_is_it_wayland(elwt) {
                                                window.pre_present_notify();
                                            }
                                            buffer.present().unwrap();
                                            frame_is_actually_queued_by_us = false;
                                            // println!("frame time: {} us", prev_frame_time_us);
                                            // println!("frame pace: {} us", frame_pace_us);
                                            if okay_but_is_it_wayland(elwt) {
                                                window.request_redraw();
                                            }
                                        }
                                    }
                                },
                                winit::event::WindowEvent::CloseRequested => {
                                    elwt.exit();
                                },
                                _ => {},
                            }
                        },
                        winit::event::Event::AboutToWait => {
                            if let Some(monitor) = window.current_monitor() {
                                if let Some(refresh_rate) = monitor.refresh_rate_millihertz() {
                                    frame_interval_milli_hertz = refresh_rate;
                                }
                            }
                            if okay_but_is_it_wayland(elwt) {
                                elwt.set_control_flow(winit::event_loop::ControlFlow::Wait);
                            } else {
                                let now = Instant::now();
                                if now >= next_frame_deadline {
                                    if last_call_to_present_instant > next_frame_deadline {
                                    } else {
                                        frame_is_actually_queued_by_us = true;  
                                        window.request_redraw();
                                    }
                                    if now - next_frame_deadline > Duration::from_millis(250) {
                                        next_frame_deadline = Instant::now() + Duration::from_secs(1000) / frame_interval_milli_hertz;
                                    } else {
                                        while now >= next_frame_deadline {
                                            next_frame_deadline += Duration::from_secs(1000) / frame_interval_milli_hertz;
                                        }
                                    }
                                }
                                elwt.set_control_flow(winit::event_loop::ControlFlow::WaitUntil(next_frame_deadline));
                            }
                        },
                        _ => (),
                    }
                }
            },
        }
    }).unwrap();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
    }
}
