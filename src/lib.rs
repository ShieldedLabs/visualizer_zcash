
use other_file::*;
mod other_file;

const TURN_OFF_HASH_BASED_LAZY_RENDER: usize = 0;

use std::{alloc::{alloc, dealloc, Layout}, hash::Hasher, hint::spin_loop, mem::transmute, ptr::{copy_nonoverlapping, slice_from_raw_parts}, rc::Rc, sync::{atomic::{AtomicU32, Ordering}, Barrier}, time::{Duration, Instant}};
use twox_hash::xxhash3_64;
use winit::dpi::Size;

use rustybuzz::{shape, Face as RbFace, UnicodeBuffer};
use swash::{scale::ScaleContext, FontRef};

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

                let is_last_time       = (*p_thread_context).is_last_time;
                let work_count         = (*p_thread_context).work_unit_count as usize;
                let work_user_pointer  = (*p_thread_context).work_user_pointer;
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

// barrier()

// while (work_a_left)
// {
//     work_a()
// }

// while (work_b_left)
// {
//     work_a()
// }

// barrier()


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

fn draw_measure_text_line(draw_ctx: &DrawCtx, text_height: isize, text_line: &str) -> isize {
    let rb_face = RbFace::from_slice(SOURCE_SERIF, 0).expect("bad font");
    let swash_font = FontRef::from_index(SOURCE_SERIF, 0).expect("font ref");

    let mut buf = UnicodeBuffer::new();
    buf.set_direction(rustybuzz::Direction::LeftToRight);
    buf.push_str(text_line);
    buf.set_direction(rustybuzz::Direction::LeftToRight);

    let shaped = shape(&rb_face, &[], buf);
    let infos = shaped.glyph_infos();
    let poss = shaped.glyph_positions();
    assert_eq!(infos.len(), poss.len());

    let target_px_height: usize = text_height as usize;
    let units_per_em: f32;
    let ppem;
    let glyph_row_shift: usize;
    {
        let m = swash_font.metrics(&[]);
        units_per_em = m.units_per_em as f32;
        ppem = target_px_height as f32 / ((m.ascent + m.descent + m.leading) / units_per_em);
        glyph_row_shift = (((m.max_width / units_per_em) * ppem).ceil() as usize).next_power_of_two().trailing_zeros() as usize;
    }

    poss.iter().map(|g_pos| {
        let px_advance = (((g_pos.x_advance as f32 / units_per_em) * ppem).ceil() as usize).min(1usize << glyph_row_shift);
        px_advance
    }).reduce(|acc, a| acc + a).unwrap() as isize
}

fn draw_text_line(draw_ctx: &DrawCtx, x: isize, text_y: isize, text_height: isize, text_line: &str, color: u32) {
    unsafe {
        if text_height <= 0 { return; }

        let mut found_font = std::ptr::null_mut();
        for i in 0..*draw_ctx.draw_command_count {
            let check = draw_ctx.font_tracker_buffer.add(i);
            if (*check).target_px_height == text_height as usize {
                found_font = check;
                break;
            }
        }

        let rb_face = RbFace::from_slice(SOURCE_SERIF, 0).expect("bad font");
        let swash_font = FontRef::from_index(SOURCE_SERIF, 0).expect("font ref");

        if found_font == std::ptr::null_mut() {
            found_font = draw_ctx.font_tracker_buffer.add(*draw_ctx.font_tracker_count);
            *draw_ctx.font_tracker_count += 1;

            // init the font
            let target_px_height: usize = text_height as usize;
            let units_per_em: f32;
            let ppem;
            let glyph_row_shift: usize;
            let baseline_y: usize;
            let max_glyph_count;
            {
                let m = swash_font.metrics(&[]);
                units_per_em = m.units_per_em as f32;
                ppem = target_px_height as f32 / ((m.ascent + m.descent + m.leading) / units_per_em);
                glyph_row_shift = (((m.max_width / units_per_em) * ppem).ceil() as usize).next_power_of_two().trailing_zeros() as usize;
                baseline_y = ((m.descent / units_per_em) * ppem).ceil() as usize;
                max_glyph_count = m.glyph_count;
            }
            // very important the buffers do not move ever
            let mut new_tracker = FontTracker {
                target_px_height,
                units_per_em,
                ppem,
                glyph_row_shift,
                baseline_y,
                max_glyph_count,
                row_buffers: Vec::new(),
                cached_bitmaps_counter: 0,
                cached_bitmap_widths: Vec::with_capacity(max_glyph_count as usize),
                glyph_to_bitmap_index: Vec::new(),
            };
            for _ in 0..target_px_height { new_tracker.row_buffers.push(Vec::with_capacity((max_glyph_count as usize) << glyph_row_shift)); }
            for _ in 0..max_glyph_count { new_tracker.glyph_to_bitmap_index.push(u16::MAX); }
            copy_nonoverlapping(&new_tracker as *const FontTracker, found_font, size_of::<FontTracker>());
            std::mem::forget(new_tracker);
        }
        let tracker = &mut *found_font;

        let mut buf = UnicodeBuffer::new();
        buf.set_direction(rustybuzz::Direction::LeftToRight);
        buf.push_str(text_line);
        buf.set_direction(rustybuzz::Direction::LeftToRight);

        let shaped = shape(&rb_face, &[], buf);
        let infos = shaped.glyph_infos();
        let poss = shaped.glyph_positions();
        assert_eq!(infos.len(), poss.len());

        let glyph_bitmap_run_start = draw_ctx.glyph_bitmap_run_allocator.add(*draw_ctx.glyph_bitmap_run_allocator_position);
        let mut glyph_bitmap_run_count = 0usize;

        let mut acc_x = x;
        for text_index in 0..infos.len() {
            let g_info = &infos[text_index];
            let g_pos = &poss[text_index];

            let px_advance = (((g_pos.x_advance as f32 / tracker.units_per_em) * tracker.ppem).ceil() as usize).min(1usize << tracker.glyph_row_shift);

            // TODO: Scissor
            if (acc_x + px_advance as isize) <= 0 { acc_x += px_advance as isize; continue; }
            if acc_x > draw_ctx.window_width { break; }

            if tracker.glyph_to_bitmap_index[g_info.glyph_id as usize] == u16::MAX
            {
                let bitmap_index = tracker.cached_bitmaps_counter;
                tracker.cached_bitmaps_counter += 1;
                tracker.glyph_to_bitmap_index[g_info.glyph_id as usize] = bitmap_index;

                tracker.cached_bitmap_widths.push(px_advance as u16);

                let mut _scale = ScaleContext::new();
                let mut scale = _scale.builder(swash_font).size(tracker.ppem).hint(false).build();

                let image = swash::scale::Render::new(&[
                    swash::scale::Source::Bitmap(swash::scale::StrikeWith::BestFit),
                    swash::scale::Source::Outline,
                ])
                .format(swash::zeno::Format::Alpha)
                .render(&mut scale, g_info.glyph_id as u16).unwrap();
                assert_eq!(image.content, swash::scale::image::Content::Mask);

                for y in 0..tracker.target_px_height {
                    let row_len = 1usize << tracker.glyph_row_shift;
                    let row_put = &mut tracker.row_buffers[y];
                    for x in 0..row_len {
                        let cx = (x as i32 - image.placement.left) as usize;
                        let cy = (y as i32 - tracker.target_px_height as i32 + image.placement.top + tracker.baseline_y as i32) as usize;
                        let blend: u8;
                        if (cx as u32) < image.placement.width && (cy as u32) < image.placement.height {
                            blend = image.data[image.placement.width as usize * cy + cx];
                        } else {
                            blend = 0;
                        }
                        row_put.push(blend);
                    }
                    assert_eq!(row_put.len(), row_len * tracker.cached_bitmaps_counter as usize);
                }
            }

            *glyph_bitmap_run_start.add(glyph_bitmap_run_count) = (tracker.glyph_to_bitmap_index[g_info.glyph_id as usize], acc_x as i16);
            glyph_bitmap_run_count += 1;
            acc_x += px_advance as isize;
        }

        *draw_ctx.glyph_bitmap_run_allocator_position += glyph_bitmap_run_count;

        for y in 0..tracker.target_px_height {
            let row_data = &tracker.row_buffers[y];
            let screen_y = y as isize + text_y;
            if screen_y >= 0 && screen_y < draw_ctx.window_height {
                *draw_ctx.draw_command_buffer.add(*draw_ctx.draw_command_count) = DrawCommand::TextRow { y: screen_y as u16, glyph_row_shift: tracker.glyph_row_shift as u8, color, glyph_bitmap_run: glyph_bitmap_run_start, glyph_bitmap_run_len: glyph_bitmap_run_count, row_bitmaps: row_data.as_ptr(), bitmap_widths: tracker.cached_bitmap_widths.as_ptr(), };
                *draw_ctx.draw_command_count += 1;
            }
        }
    }
}
fn draw_set_scissor(draw_ctx: &DrawCtx, x1: isize, y1: isize, x2: isize, y2: isize, color: u32) {

}
fn draw_rectangle(draw_ctx: &DrawCtx, x1: isize, y1: isize, x2: isize, y2: isize, color: u32) {
    unsafe {
        let put = draw_ctx.draw_command_buffer.add(*draw_ctx.draw_command_count);
        *draw_ctx.draw_command_count += 1;
        *put = DrawCommand::ColoredRectangle { x: x1.max(0) as u32, x2: x2.min(draw_ctx.window_width) as u32, y: y1.max(0) as u32, y2: y2.min(draw_ctx.window_height) as u32, color: color };
    }
}


#[derive(Debug)]
struct InputCtx {
    should_process_mouse_events: bool,
    inflight_mouse_events:       Vec::<(winit::event::MouseButton, winit::event::ElementState, isize, isize)>,
    inflight_keyboard_events:    Vec::<(winit::keyboard::KeyCode, winit::event::ElementState)>,

    mouse_x: isize,
    mouse_y: isize,
    mouse_events_last_frame: [(bool, isize, isize); 5],
    mouse_events_this_frame: [(bool, isize, isize); 5],

    keyboard_events_last_frame: [bool; winit::keyboard::KeyCode::F35 as usize],
    keyboard_events_this_frame: [bool; winit::keyboard::KeyCode::F35 as usize],
}

impl InputCtx {
    fn key_pressed(&self, key: winit::keyboard::KeyCode) -> bool {
        let idx = key as usize;
        return self.keyboard_events_last_frame[idx] && !self.keyboard_events_this_frame[idx];
    }

    fn key_held(&self, key: winit::keyboard::KeyCode) -> bool {
        let idx = key as usize;
        return self.keyboard_events_last_frame[idx] && self.keyboard_events_this_frame[idx];
    }

    fn mouse_pressed(&self, button: winit::event::MouseButton) -> (bool, isize, isize) {
        let state_last_frame = match button {
            winit::event::MouseButton::Left    => self.mouse_events_last_frame[0],
            winit::event::MouseButton::Right   => self.mouse_events_last_frame[1],
            winit::event::MouseButton::Middle  => self.mouse_events_last_frame[2],
            winit::event::MouseButton::Back    => self.mouse_events_last_frame[3],
            winit::event::MouseButton::Forward => self.mouse_events_last_frame[4],
            _ => (false, 0, 0) ,
        };
        let state_this_frame = match button {
            winit::event::MouseButton::Left    => self.mouse_events_this_frame[0],
            winit::event::MouseButton::Right   => self.mouse_events_this_frame[1],
            winit::event::MouseButton::Middle  => self.mouse_events_this_frame[2],
            winit::event::MouseButton::Back    => self.mouse_events_this_frame[3],
            winit::event::MouseButton::Forward => self.mouse_events_this_frame[4],
            _ => (false, 0, 0) ,
        };

        let dx = state_this_frame.1.abs_diff(state_last_frame.1) as isize;
        let dy = state_this_frame.2.abs_diff(state_last_frame.2) as isize;
        return (state_this_frame.0 && !state_this_frame.0, dx, dy);
    }

    fn mouse_held(&self, button: winit::event::MouseButton) -> (bool, isize, isize) {
        let state_last_frame = match button {
            winit::event::MouseButton::Left    => self.mouse_events_last_frame[0],
            winit::event::MouseButton::Right   => self.mouse_events_last_frame[1],
            winit::event::MouseButton::Middle  => self.mouse_events_last_frame[2],
            winit::event::MouseButton::Back    => self.mouse_events_last_frame[3],
            winit::event::MouseButton::Forward => self.mouse_events_last_frame[4],
            _ => (false, 0, 0) ,
        };
        let state_this_frame = match button {
            winit::event::MouseButton::Left    => self.mouse_events_this_frame[0],
            winit::event::MouseButton::Right   => self.mouse_events_this_frame[1],
            winit::event::MouseButton::Middle  => self.mouse_events_this_frame[2],
            winit::event::MouseButton::Back    => self.mouse_events_this_frame[3],
            winit::event::MouseButton::Forward => self.mouse_events_this_frame[4],
            _ => (false, 0, 0) ,
        };

        let dx = state_this_frame.1.abs_diff(state_last_frame.1) as isize;
        let dy = state_this_frame.2.abs_diff(state_last_frame.2) as isize;
        return (state_this_frame.0 && state_this_frame.0, dx, dy);
    }
}

struct FontTracker {
    target_px_height: usize,
    units_per_em: f32,
    ppem: f32,
    glyph_row_shift: usize,
    baseline_y: usize,
    max_glyph_count: u16,
    row_buffers: Vec<Vec<u8>>,
    cached_bitmaps_counter: u16,
    cached_bitmap_widths: Vec<u16>,
    glyph_to_bitmap_index: Vec<u16>,
}

struct DrawCtx {
    window_width: isize,
    window_height: isize,
    draw_command_buffer: *mut DrawCommand,
    draw_command_count: *mut usize,
    glyph_bitmap_run_allocator: *mut (u16, i16),
    glyph_bitmap_run_allocator_position: *mut usize,
    font_tracker_buffer: *mut FontTracker,
    font_tracker_count: *mut usize,
}

#[derive(Clone, Copy, Debug)]
enum DrawCommand {
    ColoredRectangle {
        x: u32,
        x2: u32,
        y: u32,
        y2: u32,
        color: u32,
    },
    TextRow {
        y: u16,
        glyph_row_shift: u8,
        color: u32,
        glyph_bitmap_run: *const (u16, i16),
        glyph_bitmap_run_len: usize,
        row_bitmaps: *const u8,
        bitmap_widths: *const u16, // TODO not this because scissor
    },
    PixelLineXDef { // will draw one pixel per x
        x1: u16, // x1 must be less than x2
        y1: u16,
        x2: u16,
        y2: u16,
        color: u32,
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

pub static SOURCE_SERIF: &[u8] = include_bytes!("../assets/source_serif_4.ttf");
pub static GOHUM_PIXEL: &[u8] = include_bytes!("../assets/gohum_pixel.ttf");

pub fn main_thread_run_program() {
    // Create window + event loop.
    let event_loop = winit::event_loop::EventLoop::new().unwrap();

    let mut frame_interval_milli_hertz = 60000;
    let mut next_frame_deadline = Instant::now() + Duration::from_secs(1000) / frame_interval_milli_hertz;
    let mut prev_frame_time_us = 0u64;
    let mut last_call_to_present_instant = Instant::now();
    let mut frame_is_actually_queued_by_us = false;
    let mut wayland_dropped_a_frame_on_purpose_counter = 0usize;

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
    let mut whole_screen_hash = 0u64;

    let mut input_ctx = InputCtx {
        should_process_mouse_events: true,
        inflight_mouse_events: Vec::new(),
        inflight_keyboard_events: Vec::new(),

        mouse_events_last_frame: [(false, 0, 0); _],
        mouse_events_this_frame: [(false, 0, 0); _],

        keyboard_events_last_frame: [false; _],
        keyboard_events_this_frame: [false; _],

        mouse_x: 0,
        mouse_y: 0,
    };

    let mut t: f64 = 0.0;

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
                                winit::event::WindowEvent::CursorEntered { device_id } |
                                winit::event::WindowEvent::CursorLeft    { device_id } => {
                                    input_ctx.should_process_mouse_events = event == winit::event::WindowEvent::CursorEntered{ device_id };
                                },
                                winit::event::WindowEvent::CursorMoved { device_id, position } => {
                                    input_ctx.mouse_x = position.x as isize;
                                    input_ctx.mouse_y = position.y as isize;
                                },
                                winit::event::WindowEvent::MouseInput { device_id, state, button } => {
                                    if input_ctx.should_process_mouse_events {
                                        input_ctx.inflight_mouse_events.push((button, state, input_ctx.mouse_x, input_ctx.mouse_y));
                                    }
                                },
                                winit::event::WindowEvent::KeyboardInput { device_id, event, is_synthetic } => {
                                    match event.physical_key {
                                        winit::keyboard::PhysicalKey::Code(kc) => {
                                            input_ctx.inflight_keyboard_events.push((kc, event.state));
                                        }

                                        _ => {},
                                    }
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
                                            // INPUT
                                            //////////////////////////////
                                            input_ctx.mouse_events_last_frame.copy_from_slice(&input_ctx.mouse_events_this_frame);
                                            for e in input_ctx.mouse_events_this_frame.iter_mut() { *e = (false, 0, 0); }
                                            for (button, state, x, y) in &input_ctx.inflight_mouse_events {
                                                let ptr = match button {
                                                    winit::event::MouseButton::Left    => &mut input_ctx.mouse_events_this_frame[0],
                                                    winit::event::MouseButton::Right   => &mut input_ctx.mouse_events_this_frame[1],
                                                    winit::event::MouseButton::Middle  => &mut input_ctx.mouse_events_this_frame[2],
                                                    winit::event::MouseButton::Back    => &mut input_ctx.mouse_events_this_frame[3],
                                                    winit::event::MouseButton::Forward => &mut input_ctx.mouse_events_this_frame[4],
                                                    _ => { continue; },
                                                };

                                                *ptr = (state.is_pressed(), *x, *y);
                                            }

                                            input_ctx.keyboard_events_last_frame.copy_from_slice(&input_ctx.keyboard_events_this_frame);
                                            for e in input_ctx.keyboard_events_this_frame.iter_mut() { *e = false; }
                                            for (key, state) in &input_ctx.inflight_keyboard_events {
                                                input_ctx.keyboard_events_this_frame[*key as usize] = state.is_pressed();
                                            }

                                            input_ctx.inflight_mouse_events.clear();
                                            input_ctx.inflight_keyboard_events.clear();


                                            //////////////////////////////
                                            // UPDATE
                                            //////////////////////////////
                                            let dt = 1000.0 / (frame_interval_milli_hertz as f64);

                                            t += dt;
                                            let t = if (t as u64 / 10) & 1 != 0 { t } else { 0.0 };
                                            let (fx, _fy) = loop_curve(t.powf(1.6));
                                            let (_fx, fy) = loop_curve(t.powf(1.7));
                                            let ix = (500.0 + fx*40.0) as u32;
                                            let iy = (300.0 + fy*30.0) as u32;

                                            let mouse_box_x = input_ctx.mouse_x as u32;
                                            let mouse_box_y = input_ctx.mouse_y as u32;

                                            let mut draw_commands = Vec::new();
                                            draw_commands.push(DrawCommand::ColoredRectangle { x: 0, x2: window_width as u32, y: 0, y2: window_height as u32, color: 0x222222 });
                                            draw_commands.push(DrawCommand::ColoredRectangle { x: ix, x2: ix + 50, y: iy, y2: iy+40, color: 0x3357FF });

                                            draw_commands.push(DrawCommand::ColoredRectangle { x: mouse_box_x, x2: mouse_box_x + 100, y: mouse_box_y, y2: mouse_box_y+50, color: 0xFF3366 });

                                            let rb_face = RbFace::from_slice(SOURCE_SERIF, 0).expect("bad font");
                                            let swash_font = FontRef::from_index(SOURCE_SERIF, 0).expect("font ref");

                                            // DO NOT SET TARGET SIZE TO ZERO. Swash gets very flow in that case.
                                            let target_px_height: usize = mouse_box_y.min(512).max(1) as usize;
                                            let units_per_em: f32;
                                            let ppem;
                                            let glyph_row_shift: usize;
                                            let baseline_y: usize;
                                            let max_glyph_count;
                                            {
                                                let m = swash_font.metrics(&[]);
                                                units_per_em = m.units_per_em as f32;
                                                ppem = target_px_height as f32 / ((m.ascent + m.descent + m.leading) / units_per_em);
                                                glyph_row_shift = (((m.max_width / units_per_em) * ppem).ceil() as usize).next_power_of_two().trailing_zeros() as usize;
                                                baseline_y = ((m.descent / units_per_em) * ppem).ceil() as usize;
                                                max_glyph_count = m.glyph_count;
                                            }
                                            //println!("h: {} -> ppem: {} and 1 << glyph_row_shift: {} baseline_y: {}", target_px_height, ppem, 1 << glyph_row_shift, baseline_y);

                                            // temp
                                            let mut glyph_bitmap_run: Vec<(u16, i16)> = Vec::new();


                                            let mut row_buffers: Vec<Vec<u8>> = Vec::new();
                                            let mut cached_bitmaps_counter = 0;
                                            let mut cached_bitmap_widths = Vec::new();
                                            let mut glyph_to_bitmap_index = Vec::new();
                                            for _ in 0..target_px_height { row_buffers.push(Vec::new()); }
                                            for _ in 0..max_glyph_count { glyph_to_bitmap_index.push(u16::MAX); }
                                            {
                                                let text_line = "Salvē | Hello | Привет | 你好 <- No Chinese because the fonts are very big.";

                                                let mut buf = UnicodeBuffer::new();
                                                buf.set_direction(rustybuzz::Direction::LeftToRight);
                                                buf.push_str(text_line);
                                                buf.set_direction(rustybuzz::Direction::LeftToRight);

                                                let shaped = shape(&rb_face, &[], buf);
                                                let infos = shaped.glyph_infos();
                                                let poss = shaped.glyph_positions();
                                                assert_eq!(infos.len(), poss.len());

                                                let mut acc_x = -50;
                                                for text_index in 0..infos.len() {
                                                    let g_info = &infos[text_index];
                                                    let g_pos = &poss[text_index];
                                                    if acc_x > window_width as i16 { break; }

                                                    if glyph_to_bitmap_index[g_info.glyph_id as usize] == u16::MAX
                                                    {
                                                        let bitmap_index = cached_bitmaps_counter;
                                                        cached_bitmaps_counter += 1;
                                                        glyph_to_bitmap_index[g_info.glyph_id as usize] = bitmap_index;

                                                        let px_advance = (((g_pos.x_advance as f32 / units_per_em) * ppem).ceil() as usize).min(1usize << glyph_row_shift);
                                                        cached_bitmap_widths.push(px_advance as u16);

                                                        // 3) Set up a scaler (swash) for rasterizing glyph images
                                                        let mut _scale = ScaleContext::new();
                                                        let mut scale = _scale.builder(swash_font).size(ppem).hint(false).build();

                                                        let image = swash::scale::Render::new(&[
                                                            swash::scale::Source::Bitmap(swash::scale::StrikeWith::BestFit),
                                                            swash::scale::Source::Outline,
                                                        ])
                                                        .format(swash::zeno::Format::Alpha)
                                                        .render(&mut scale, g_info.glyph_id as u16).unwrap();
                                                        assert_eq!(image.content, swash::scale::image::Content::Mask);

                                                        for y in 0..target_px_height {
                                                            let row_len = 1usize << glyph_row_shift;
                                                            let row_put = &mut row_buffers[y];
                                                            for x in 0..row_len {
                                                                let cx = (x as i32 - image.placement.left) as usize;
                                                                let cy = (y as i32 - target_px_height as i32 + image.placement.top + baseline_y as i32) as usize;
                                                                let blend: u8;
                                                                if (cx as u32) < image.placement.width && (cy as u32) < image.placement.height {
                                                                    blend = image.data[image.placement.width as usize * cy + cx];
                                                                } else {
                                                                    blend = 0;
                                                                }
                                                                row_put.push(blend);
                                                            }
                                                            assert_eq!(row_put.len(), row_len * cached_bitmaps_counter as usize);
                                                        }
                                                    }

                                                    glyph_bitmap_run.push((glyph_to_bitmap_index[g_info.glyph_id as usize], acc_x));
                                                    let px_advance = (((g_pos.x_advance as f32 / units_per_em) * ppem).ceil() as usize).min(1usize << glyph_row_shift);
                                                    acc_x += px_advance as i16;
                                                }

                                                for y in 0..target_px_height {
                                                    let row_data = &row_buffers[y];
                                                    draw_commands.push(DrawCommand::TextRow { y: y as u16 + 200, glyph_row_shift: glyph_row_shift as u8, color: 0xFFFFFFu32, glyph_bitmap_run: glyph_bitmap_run.as_ptr(), glyph_bitmap_run_len: glyph_bitmap_run.len(), row_bitmaps: row_data.as_ptr(), bitmap_widths: cached_bitmap_widths.as_ptr(), });
                                                }
                                            }

                                            let mut _draw_command_count = 0usize;
                                            let mut _glyph_bitmap_run_allocator_position = 0usize;
                                            let mut _font_tracker_count = 0usize;

                                            // Do draw stuff with the good API
                                            let mut draw_ctx = unsafe { DrawCtx {
                                                window_width: window_width as isize,
                                                window_height: window_height as isize,
                                                draw_command_buffer: alloc(Layout::array::<DrawCommand>(8192).unwrap()) as *mut DrawCommand,
                                                draw_command_count: (&mut _draw_command_count) as *mut usize,
                                                glyph_bitmap_run_allocator: alloc(Layout::array::<(u16, i16)>(8192).unwrap()) as *mut (u16, i16),
                                                glyph_bitmap_run_allocator_position: (&mut _glyph_bitmap_run_allocator_position) as *mut usize,
                                                font_tracker_buffer: alloc(Layout::array::<FontTracker>(8192).unwrap()) as *mut FontTracker,
                                                font_tracker_count: (&mut _font_tracker_count) as *mut usize,
                                            }};

                                            let should_quit = demo_of_rendering_stuff_with_context_that_allocates_in_the_background(&draw_ctx, &input_ctx);
                                            if should_quit {
                                                elwt.exit();
                                            }

                                            // adapter
                                            draw_commands.extend(std::slice::from_raw_parts(draw_ctx.draw_command_buffer as *const DrawCommand, *draw_ctx.draw_command_count).iter());

                                            draw_commands.push(DrawCommand::PixelLineXDef { x1: 50, y1: 500, x2: mouse_box_x as u16, y2: mouse_box_y as u16, color: 0xffbb00 });

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

                                            fn blend_u32(color_1: u32, color_2: u32, blend: u32) -> u32 {
                                                let b1 = (color_1 >> 0) & 0xff;
                                                let g1 = (color_1 >> 8) & 0xff;
                                                let r1 = (color_1 >> 16) & 0xff;
                                                let b2 = (color_2 >> 0) & 0xff;
                                                let g2 = (color_2 >> 8) & 0xff;
                                                let r2 = (color_2 >> 16) & 0xff;
                                                let b = (b1 as u32 * (255 - blend) + b2 * blend) / 255;
                                                let g = (g1 as u32 * (255 - blend) + g2 * blend) / 255;
                                                let r = (r1 as u32 * (255 - blend) + r2 * blend) / 255;
                                                r << 16 | g << 8 | b
                                            }
                                            fn linear_to_srgb_one_channel_float(linear: f32) -> f32 {
                                                if linear <= 0.0031308 {
                                                    12.92 * linear
                                                } else {
                                                    1.055 * linear.powf(1.0 / 2.4) - 0.055
                                                }
                                            }
                                            fn srgb_to_linear_one_channel_float(srgb: f32) -> f32 {
                                                if srgb <= 0.04045 {
                                                    srgb / 12.92
                                                } else {
                                                    ((srgb + 0.055) / 1.055).powf(2.4)
                                                }
                                            }

                                            let mut ups = ExecuteCommandBufferOnTilesCtx {
                                                render_target_0,
                                                render_target_stride: draw_area_pixel_wide,
                                                window_width,
                                                window_height,
                                                saved_tile_hashes: saved_tile_hashes.as_mut_ptr(),
                                                draw_commands: draw_commands.as_ptr(),
                                                draw_command_count: draw_commands.len(),
                                            };
                                            dennis_parallel_for(p_thread_context, false, tiles_wide*tiles_wide, &ups as *const ExecuteCommandBufferOnTilesCtx as usize,
                                                |thread_id: usize, work_id: usize, work_count: usize, user_pointer: usize| {
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
                                                                    },
                                                                    DrawCommand::TextRow { y, glyph_row_shift, color, glyph_bitmap_run, glyph_bitmap_run_len, row_bitmaps, bitmap_widths } => {
                                                                        if (y as u32) < tile_pixel_y || (y as u32) >= tile_pixel_y2 { continue; }
                                                                        for i in 0..glyph_bitmap_run_len {
                                                                            let (lookup_index, start_x) = *glyph_bitmap_run.add(i);
                                                                            let width = *bitmap_widths.add(lookup_index as usize) as usize;

                                                                            if (start_x as isize + width as isize - 1) < tile_pixel_x as isize { continue; }
                                                                            if (start_x as isize) >= tile_pixel_x2 as isize { break; }
                                                                            hasher.write_u64(0x8936730958944);
                                                                            hasher.write_u16(lookup_index);
                                                                            hasher.write_i16(start_x);
                                                                            hasher.write_u16(y);
                                                                            hasher.write_usize(row_bitmaps as usize);
                                                                            hasher.write_usize(bitmap_widths as usize);
                                                                            // TODO rethink, font id?
                                                                            // TODO color
                                                                            if should_draw == false { continue; }

                                                                            let mut copy_data = row_bitmaps.byte_add((lookup_index as usize) << glyph_row_shift);
                                                                            let mut put_data = ctx.render_target_0.byte_add((y as usize) << (pixel_row_shift+2)).byte_offset(start_x as isize *4);

                                                                            let mut x1 = start_x as isize;
                                                                            let x2 = (start_x as isize + width as isize).min(tile_pixel_x2 as isize);
                                                                            if x1 < tile_pixel_x as isize {
                                                                                copy_data = copy_data.byte_add((tile_pixel_x as isize - x1) as usize);
                                                                                put_data = put_data.byte_add((tile_pixel_x as isize - x1) as usize * 4);
                                                                                x1 = tile_pixel_x as isize;
                                                                            }
                                                                            let len = x2 - x1;
                                                                            debug_assert!(len != 0);
                                                                            for _ in 0..len {
                                                                                let blend = *copy_data as u32;
                                                                                copy_data = copy_data.byte_add(1);
                                                                                *(put_data as *mut u32) = blend_u32(*(put_data as *mut u32), color, blend);
                                                                                put_data = put_data.byte_add(4);
                                                                            }
                                                                        }
                                                                    },
                                                                    DrawCommand::PixelLineXDef { x1, y1, x2, y2, color } => {
                                                                        let start_x = (x1 as u32).max(tile_pixel_x);
                                                                        let end_x = (x2 as u32).min(tile_pixel_x2);
                                                                        if start_x >= end_x { continue; }
                                                                        let dy = (y2 as f32 - y1 as f32) / (x2 - x1) as f32;
                                                                        hasher.write_u64(0x75634593484);
                                                                        hasher.write_u16(x1);
                                                                        hasher.write_u16(x2);
                                                                        hasher.write_u16(y1);
                                                                        hasher.write_u16(y2);
                                                                        hasher.write_u32(color);
                                                                        hasher.write_u32(dy.to_bits());
                                                                        if should_draw == false { continue; }
                                                                        for real_x in start_x..end_x {
                                                                            // ALT: anti-aliasing version
                                                                            let fy = y1 as f32 + dy * (real_x - x1 as u32) as f32;
                                                                            let fy1 = fy.floor();
                                                                            let fy2 = fy.ceil();
                                                                            let iy1 = fy1.round() as u32;
                                                                            let iy2 = fy2.round() as u32;
                                                                            let blend = (fy - fy2).abs();
                                                                            let blend1 = (blend) * 255.0;
                                                                            let blend2 = (1.0 - blend) * 255.0;

                                                                            if iy1 >= tile_pixel_y && iy1 < tile_pixel_y2 {
                                                                                let pixel = ctx.render_target_0.byte_add(((real_x + (iy1 << pixel_row_shift)) as usize) << 2);
                                                                                *(pixel as *mut u32) = blend_u32(*(pixel as *mut u32), color, blend1 as u32);
                                                                            }
                                                                            if iy2 >= tile_pixel_y && iy2 < tile_pixel_y2 {
                                                                                let pixel = ctx.render_target_0.byte_add(((real_x + (iy2 << pixel_row_shift)) as usize) << 2);
                                                                                *(pixel as *mut u32) = blend_u32(*(pixel as *mut u32), color, blend2 as u32);
                                                                            }
                                                                        }
                                                                    },
                                                                }
                                                            }
                                                            got_hash = hasher.finish();
                                                        }
                                                    }
                                                });

                                            let need_buffer_flip;
                                            {
                                                let mut hasher = xxhash3_64::Hasher::new();
                                                hasher.write_usize(window_width);
                                                hasher.write_usize(window_height);
                                                for th in &saved_tile_hashes { hasher.write_u64(*th); }
                                                let new_hash = hasher.finish();
                                                need_buffer_flip = whole_screen_hash != new_hash;
                                                whole_screen_hash = new_hash;
                                            }

                                            prev_frame_time_us = begin_frame_instant.elapsed().as_micros() as u64;

                                            struct EndOfFrameBlitCtx {
                                                render_target_0: *mut u8,
                                                display_buffer: *mut u8,
                                                row_count: usize,
                                                render_target_stride: usize,
                                                display_buffer_stride: usize,
                                            }
                                            let ups = EndOfFrameBlitCtx {
                                                render_target_0,
                                                display_buffer: final_output_blit_buffer,
                                                row_count: window_height,
                                                render_target_stride: draw_area_pixel_wide,
                                                display_buffer_stride: window_width,
                                            };
                                            dennis_parallel_for(p_thread_context, true, (need_buffer_flip as usize)*((window_height + 32 - 1) / 32), &ups as *const EndOfFrameBlitCtx as usize,
                                            |thread_id: usize, work_id: usize, work_count: usize, user_pointer: usize| {
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
                                            });

                                            let frame_pace_us = last_call_to_present_instant.elapsed().as_micros() as u64;
                                            last_call_to_present_instant = Instant::now();
                                            if need_buffer_flip {
                                                if okay_but_is_it_wayland(elwt) {
                                                    window.pre_present_notify();
                                                }
                                                buffer.present().unwrap();
                                            }
                                            frame_is_actually_queued_by_us = false;
                                            // println!("frame time: {} us", prev_frame_time_us);
                                            // println!("frame pace: {} us", frame_pace_us);
                                            if okay_but_is_it_wayland(elwt) {
                                                if need_buffer_flip {
                                                    window.request_redraw();
                                                } else {
                                                    wayland_dropped_a_frame_on_purpose_counter = 2;
                                                }
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
                                if wayland_dropped_a_frame_on_purpose_counter == 2 {
                                    wayland_dropped_a_frame_on_purpose_counter = 1;
                                    elwt.set_control_flow(winit::event_loop::ControlFlow::WaitUntil(Instant::now() + Duration::from_secs(1000) / frame_interval_milli_hertz));
                                } else if wayland_dropped_a_frame_on_purpose_counter == 1 {
                                    wayland_dropped_a_frame_on_purpose_counter = 0;
                                    window.request_redraw();
                                    elwt.set_control_flow(winit::event_loop::ControlFlow::Wait);
                                } else {
                                    elwt.set_control_flow(winit::event_loop::ControlFlow::Wait);
                                }
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
