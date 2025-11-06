use twox_hash::XxHash3_64;

use super::*;

struct OnScreenBc {
    x: f64,
    y: f64,
}
pub struct VizState {
    camera_x: f64,
    camera_y: f64,
    zoom: f64,
    on_screen_bcs: Vec<OnScreenBc>,
    sway_t : f64,
}
pub fn viz_gui_init() -> VizState {
    VizState {
        camera_x: 0.0,
        camera_y: 0.0,
        zoom: 1.0,
        on_screen_bcs: vec![
            OnScreenBc { x: -5.0, y: -10.0 },
            OnScreenBc { x: -5.0, y: -20.0 },
            OnScreenBc { x: -5.0, y: -30.0 },
            OnScreenBc { x: -5.0, y: -40.0 },
            OnScreenBc { x: -5.0, y: -50.0 },
        ],
        sway_t: 0.0,
    }
}
pub fn viz_gui_anything_happened_at_all(viz_state: &mut VizState) -> bool {
    true
}
pub fn viz_gui_draw_the_stuff_for_the_things(viz_state: &mut VizState, draw_ctx: &DrawCtx, dt: f64, input_ctx: &InputCtx) {

    viz_state.sway_t += dt;
    for i in 0..viz_state.on_screen_bcs.len() {
        viz_state.on_screen_bcs[i].x += f64::sin(viz_state.sway_t*2.0 + i as f64 / 15.0) / 4.0;
    }

    if input_ctx.key_pressed(KeyCode::F2) {
        viz_state.zoom *= 0.9;
    }
    if input_ctx.key_pressed(KeyCode::F3) {
        viz_state.zoom /= 0.9;
    }

    // origin
    let origin_x = draw_ctx.window_width / 2;
    let origin_y = draw_ctx.window_height - 100;
    let screen_unit = 10.0 * viz_state.zoom;
    draw_ctx.circle(origin_x as f32, origin_y as f32, (screen_unit/2.0) as f32, 0xff_0000bb);

    for i in 0..viz_state.on_screen_bcs.len() {
        let x = viz_state.on_screen_bcs[i].x;
        let y = viz_state.on_screen_bcs[i].y;
        draw_ctx.circle(origin_x as f32 + (x*screen_unit) as f32, origin_y as f32 + (y*screen_unit) as f32, screen_unit as f32, 0xff_ffffff);
        draw_ctx.mono_text_line(origin_x + ((x + 1.5)*screen_unit) as isize, origin_y + ((y - 0.5)*screen_unit) as isize, screen_unit as isize, &format!("{:x}", XxHash3_64::oneshot(format!("bhsaerht{}", i).as_bytes())), 0xFF_ffffff);

        if i > 0 {
            let px = viz_state.on_screen_bcs[i-1].x;
            let py = viz_state.on_screen_bcs[i-1].y;
            let dx = px-x;
            let dy = py-y - 2.0;
            draw_ctx.arrow(
                origin_x + (x*screen_unit) as isize,
                origin_y + ((y+1.5)*screen_unit) as isize,
                origin_x + ((x + dx)*screen_unit) as isize,
                origin_y + ((y + dy)*screen_unit) as isize,
                screen_unit/3.0, 0xff_2222cc
            );
        }
    }
}

fn split_vector(mut x: f64, mut y: f64) -> (f64, f64, f64) {
    let len = f64::sqrt(x*x + y*y);
    if len < 0.0000001 { return (0.0, 0.0, 0.0); }
    (x/len, y/len, len)
}