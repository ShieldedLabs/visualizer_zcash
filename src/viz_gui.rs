use std::sync::Mutex;

use twox_hash::XxHash3_64;

use super::*;

pub static REQUESTS_TO_ZEBRA: Mutex<Option<std::sync::mpsc::Receiver<RequestToZebra>>> = Mutex::new(None);
pub static RESPONSES_FROM_ZEBRA: Mutex<Option<std::sync::mpsc::SyncSender<ResponseFromZebra>>> = Mutex::new(None);

pub struct RequestToZebra {
    rtype: u8,
}
impl RequestToZebra {
    pub fn _0() -> Self {
        RequestToZebra {
            rtype: 0,
        }
    }
}
pub struct ResponseFromZebra {
    pub bc_tip_height: u64,
    pub bft_tip_height: u64,
}
impl ResponseFromZebra {
    pub fn _0() -> Self {
        ResponseFromZebra {
            bc_tip_height: 0,
            bft_tip_height: 0,
        }
    }
}

struct OnScreenBc {
    x: f32,
    y: f32,
}
pub struct VizState {
    camera_x: f32,
    camera_y: f32,
    zoom: f32,
    on_screen_bcs: Vec<OnScreenBc>,
    sway_t : f32,
    send_to_zebra: std::sync::mpsc::SyncSender<RequestToZebra>,
    receive_from_zebra: std::sync::mpsc::Receiver<ResponseFromZebra>,

    bc_tip_height: u64,
    bft_tip_height: u64,
}
pub fn viz_gui_init() -> VizState {
    let (me_send, zebra_receive) = std::sync::mpsc::sync_channel(128);
    let (zebra_send, me_receive) = std::sync::mpsc::sync_channel(128);

    *REQUESTS_TO_ZEBRA.lock().unwrap() = Some(zebra_receive);
    *RESPONSES_FROM_ZEBRA.lock().unwrap() = Some(zebra_send);

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
        send_to_zebra: me_send,
        receive_from_zebra: me_receive,
        bc_tip_height: 0,
        bft_tip_height: 0,
    }
}
pub fn viz_gui_anything_happened_at_all(viz_state: &mut VizState) -> bool {
    let mut anything_happened = false;

    while let Ok(message) = viz_state.receive_from_zebra.try_recv() {
        anything_happened |= viz_state.bc_tip_height != message.bc_tip_height;
        viz_state.bc_tip_height = message.bc_tip_height;
        anything_happened |= viz_state.bft_tip_height != message.bft_tip_height;
        viz_state.bft_tip_height = message.bft_tip_height;
    }

    if anything_happened == false {
        let _ = viz_state.send_to_zebra.try_send(RequestToZebra::_0());
    }
    anything_happened
}
pub(crate) fn viz_gui_draw_the_stuff_for_the_things(viz_state: &mut VizState, draw_ctx: &DrawCtx, dt: f32, input_ctx: &InputCtx) {

    viz_state.sway_t += dt;
    for i in 0..viz_state.on_screen_bcs.len() {
        viz_state.on_screen_bcs[i].x += f32::sin(viz_state.sway_t*2.0 + i as f32 / 15.0) / 4.0;
    }

    if input_ctx.key_pressed(KeyCode::F2) {
        viz_state.zoom *= 0.9;
    }
    if input_ctx.key_pressed(KeyCode::F3) {
        viz_state.zoom /= 0.9;
    }

    // origin
    let origin_x = (draw_ctx.window_width / 2) as f32;
    let origin_y = (draw_ctx.window_height - 100) as f32;
    let screen_unit = 10.0 * viz_state.zoom;
    draw_ctx.circle(origin_x as f32, origin_y as f32, (screen_unit/2.0) as f32, 0xff_0000bb);

    for i in 0..viz_state.on_screen_bcs.len() {
        let x = viz_state.on_screen_bcs[i].x;
        let y = viz_state.on_screen_bcs[i].y;
        draw_ctx.circle(origin_x as f32 + (x*screen_unit) as f32, origin_y as f32 + (y*screen_unit) as f32, screen_unit as f32, 0xff_ffffff);
        draw_ctx.mono_text_line((origin_x + (x + 1.5)*screen_unit) as f32, (origin_y + (y - 0.5)*screen_unit) as f32, screen_unit as f32, &format!("{:X}", XxHash3_64::oneshot(format!("bhsaerht{}", i).as_bytes())), 0xFF_ffffff);

        if i > 0 {
            let px = viz_state.on_screen_bcs[i-1].x;
            let py = viz_state.on_screen_bcs[i-1].y;
            let dx = px-x;
            let dy = py-y - 2.0;
            draw_ctx.arrow(
                origin_x + x*screen_unit,
                origin_y + (y+1.5)*screen_unit,
                origin_x + (x + dx)*screen_unit,
                origin_y + (y + dy)*screen_unit,
                screen_unit/3.0, 0xff_2222cc
            );
        }
    }

    draw_ctx.text_line(origin_x+2.0*screen_unit, origin_y, screen_unit*3.0, &format!("Bc Height: {} BFT Height: {}", viz_state.bc_tip_height, viz_state.bft_tip_height), 0xff_ffffff);
}

fn split_vector(mut x: f32, mut y: f32) -> (f32, f32, f32) {
    let len = f32::sqrt(x*x + y*y);
    if len < 0.0000001 { return (0.0, 0.0, 0.0); }
    (x/len, y/len, len)
}
