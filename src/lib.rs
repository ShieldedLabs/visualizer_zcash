use std::{alloc::{alloc, dealloc, Layout}, ptr::{copy_nonoverlapping, slice_from_raw_parts}, rc::Rc, sync::{atomic::{AtomicU32, Ordering}, Barrier}, time::{Duration, Instant}};
use wide::*;
use winit::dpi::Size;

const RENDER_TILE_SHIFT: usize = 7;
const RENDER_TILE_SIZE: usize = 1 << RENDER_TILE_SHIFT;
const RENDER_TILE_INTRA_MASK: usize = RENDER_TILE_SIZE.wrapping_sub(1);

struct ThreadContext {
    render_target_0: *mut u8,
    display_buffer: *mut u8,
    row_count: usize,
    render_target_stride: usize,
    display_buffer_stride: usize,
    thread_barrier_begin_blit: AtomicU32,
    wake_up_barrier: Barrier,

    work_unit_count: u32,
    work_unit_take: AtomicU32,
    work_unit_complete: AtomicU32,
    work_user_pointer: usize,
}

fn barrier_sync_spin(barrier: &AtomicU32, thread_count: u32) {
    let was = barrier.fetch_add(1, Ordering::AcqRel);
    if (was+1) % thread_count == 0 { return; }
    while barrier.load(Ordering::Acquire) < (was+1 + thread_count - 1) / thread_count * thread_count {}
}
fn barrier_sync_signal_only(barrier: &AtomicU32, thread_count: u32) {
    let was = barrier.fetch_add(1, Ordering::AcqRel);
}

fn worker_thread_loop(thread_id: usize, thread_count: usize, p_thread_context: usize) {
    let p_thread_context = p_thread_context as *mut ThreadContext;
    unsafe {
        loop {
            (*p_thread_context).wake_up_barrier.wait();
            barrier_sync_spin(&(*p_thread_context).thread_barrier_begin_blit, thread_count as u32);

            let work_count = (*p_thread_context).work_unit_count as usize;
            let work_user_pointer = (*p_thread_context).work_user_pointer;
            loop {
                let work_id = (*p_thread_context).work_unit_take.fetch_add(1, Ordering::Relaxed) as usize;
                if work_id < work_count {
                    perform_row_stride_copy(work_id, work_count, work_user_pointer);
                    
                    (*p_thread_context).work_unit_complete.fetch_add(1, Ordering::Release);
                } else {
                    break;
                }
            }
        }
    }
}

fn perform_row_stride_copy(work_id: usize, work_count: usize, user_pointer: usize) {
    unsafe {
        let p_thread_context = user_pointer as *mut ThreadContext;
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

pub fn main_thread_run_program() {
    // Create window + event loop.
    let event_loop = winit::event_loop::EventLoop::new().unwrap();
    
    let mut frame_interval_milli_hertz = 60000;
    let mut next_frame_deadline = Instant::now() + Duration::from_secs(1000) / frame_interval_milli_hertz;
    let mut prev_frame_time_us = 0u64;
    let mut last_call_to_present_instant = Instant::now();
    let mut frame_is_actually_queued_by_us = false;

    let thread_count = num_cpus::get_physical();

    let mut s_thread_context = ThreadContext {
        render_target_0: std::ptr::null_mut(),
        display_buffer: std::ptr::null_mut(),
        row_count: 0,
        render_target_stride: 0,
        display_buffer_stride: 0,
        thread_barrier_begin_blit: AtomicU32::new(0),
        wake_up_barrier: Barrier::new(thread_count),

        work_unit_count: 0,
        work_unit_take: AtomicU32::new(0),
        work_unit_complete: AtomicU32::new(0),
        work_user_pointer: 0,
    };
    let p_thread_context: *mut ThreadContext = &mut s_thread_context as *mut ThreadContext;

    for thread_id in 1..thread_count {
        let magic_int = p_thread_context as usize;
        let _ = std::thread::spawn(move || {
            println!("Started worker thread#{}...", thread_id);
            worker_thread_loop(thread_id, thread_count, magic_int);
        });
    }

    let mut byte_color: u8 = 0xaa;

    let mut window: Option<Rc<winit::window::Window>> = None;
    let mut softbuffer_context: Option<softbuffer::Context<Rc<winit::window::Window>>> = None;
    let mut softbuffer_surface: Option<softbuffer::Surface<Rc<winit::window::Window>, Rc<winit::window::Window>>> = None;
    event_loop.run(move |event, elwt| {
        match event {
            winit::event::Event::Resumed => { // Runs at startup and is where we have to do init.
                let twindow = Rc::new(elwt.create_window(
                    winit::window::WindowAttributes::default()
                    .with_title("winit + softbuffer")
                    .with_inner_size(Size::Physical(winit::dpi::PhysicalSize { width: 1024, height: 1024 }))
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
                                winit::event::WindowEvent::RedrawRequested => {
                                    if frame_is_actually_queued_by_us {
                                        unsafe {
                                            let begin_frame_instant = Instant::now();

                                            (*p_thread_context).wake_up_barrier.wait();
                                            // Tell workers: WAKE UP WAKE UP WAKE UP!!!
                                            
                                            let (window_width, window_height) = {
                                                let size = window.inner_size();
                                                (size.width as usize, size.height as usize)
                                            };
                                            softbuffer_surface.resize((window_width as u32).try_into().unwrap(), (window_height as u32).try_into().unwrap()).unwrap();
                                            
                                            let mut buffer = softbuffer_surface.buffer_mut().unwrap();
                                            let final_output_blit_buffer = buffer.as_mut_ptr() as *mut u8;
                                            let window_square: usize = window_width.max(window_height);
                                            let tiles_wide = ((window_square + RENDER_TILE_SIZE - 1) / RENDER_TILE_SIZE).next_power_of_two();
                                            let total_tiles = tiles_wide*tiles_wide;
                                            let draw_area_pixel_wide = tiles_wide * RENDER_TILE_SIZE;

                                            let pixel_row_shift = draw_area_pixel_wide.trailing_zeros();
                                            let intra_row_mask = draw_area_pixel_wide.wrapping_sub(1);

                                            let render_target_memory_layout = Layout::array::<u32>((draw_area_pixel_wide*draw_area_pixel_wide) as usize).unwrap().align_to(4096).unwrap();
                                            let render_target_0 = alloc(render_target_memory_layout);
                                            // std::ptr::write_bytes(render_target_0, byte_color, draw_area_pixel_wide*draw_area_pixel_wide*4);
                                            byte_color = byte_color.wrapping_add(1);

                                            (*p_thread_context).render_target_0 = render_target_0;
                                            (*p_thread_context).display_buffer = final_output_blit_buffer;
                                            (*p_thread_context).row_count = window_height;
                                            (*p_thread_context).render_target_stride = draw_area_pixel_wide;
                                            (*p_thread_context).display_buffer_stride = window_width;
                                            
                                            let work_count = (window_height + 32 - 1) / 32;
                                            let work_user_pointer = p_thread_context as usize;
                                            (*p_thread_context).work_unit_count = work_count as u32;
                                            (*p_thread_context).work_unit_take.store(0, Ordering::Relaxed);
                                            (*p_thread_context).work_unit_complete.store(0, Ordering::Relaxed);
                                            (*p_thread_context).work_user_pointer = work_user_pointer;

                                            barrier_sync_signal_only(&(*p_thread_context).thread_barrier_begin_blit, thread_count as u32);
                                            // This is the actual barrier to begin work. Hopefully the threads are awake by now so that we return immediately.
                                            let begin_work_instant = Instant::now();
                                            
                                            loop {
                                                let work_id = (*p_thread_context).work_unit_take.fetch_add(1, Ordering::Relaxed) as usize;
                                                if work_id < work_count {
                                                    perform_row_stride_copy(work_id, work_count, work_user_pointer);
                                                    
                                                    (*p_thread_context).work_unit_complete.fetch_add(1, Ordering::Release);
                                                } else {
                                                    break;
                                                }
                                            }

                                            while (*p_thread_context).work_unit_complete.load(Ordering::Acquire) < work_count as u32 {}
                                            let work_time_us = begin_work_instant.elapsed().as_micros();

                                            dealloc(render_target_0, render_target_memory_layout);
                                            
                                            prev_frame_time_us = begin_frame_instant.elapsed().as_micros() as u64;
                                            let frame_pace_us = last_call_to_present_instant.elapsed().as_micros() as u64;
                                            last_call_to_present_instant = Instant::now();
                                            buffer.present().unwrap();
                                            frame_is_actually_queued_by_us = false;
                                            println!("frame time: {} us  work time: {} us", prev_frame_time_us, work_time_us);
                                            //println!("frame pace: {} us", frame_pace_us);
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
