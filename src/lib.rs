use std::{rc::Rc, time::{Duration, Instant}};


pub fn main_thread_run_program() {
    // Create window + event loop.
    let event_loop = winit::event_loop::EventLoop::new().unwrap();
    
    let mut frame_interval_milli_hertz = 60000;
    let mut next_frame_deadline = Instant::now() + Duration::from_secs(1000) / frame_interval_milli_hertz;
    let mut prev_frame_time_us = 0u64;
    let mut last_call_to_present_instant = Instant::now();
    let mut frame_is_actually_queued_by_us = false;

    let mut window: Option<Rc<winit::window::Window>> = None;
    let mut softbuffer_context: Option<softbuffer::Context<Rc<winit::window::Window>>> = None;
    let mut softbuffer_surface: Option<softbuffer::Surface<Rc<winit::window::Window>, Rc<winit::window::Window>>> = None;
    event_loop.run(move |event, elwt| {
        match event {
            winit::event::Event::Resumed => { // Runs at startup and is where we have to do init.
                let twindow = Rc::new(elwt.create_window(
                    winit::window::WindowAttributes::default()
                    .with_title("winit + softbuffer")
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
                                        let begin_frame_instant = Instant::now();
                                        let (window_width, window_height) = {
                                            let size = window.inner_size();
                                            (size.width, size.height)
                                        };
                                        softbuffer_surface.resize(window_width.try_into().unwrap(), window_height.try_into().unwrap()).unwrap();

                                        let mut buffer = softbuffer_surface.buffer_mut().unwrap();
                                        for index in 0..(window_width * window_height) {
                                            let y = index / window_width;
                                            let x = index % window_width;
                                            let red = x % 255;
                                            let green = y % 255;
                                            let blue = (x * y) % 255;

                                            buffer[index as usize] = blue | (green << 8) | (red << 16);
                                        }

                                        prev_frame_time_us = begin_frame_instant.elapsed().as_micros() as u64;
                                        let frame_pace_us = last_call_to_present_instant.elapsed().as_micros() as u64;
                                        last_call_to_present_instant = Instant::now();
                                        buffer.present().unwrap();
                                        frame_is_actually_queued_by_us = false;
                                        println!("frame time: {} us", prev_frame_time_us);
                                        //println!("frame pace: {} us", frame_pace_us);
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
