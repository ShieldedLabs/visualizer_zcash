use std::{collections::HashMap, hash::Hash, sync::Mutex};

use twox_hash::XxHash3_64;
use winit::event::MouseButton;

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

struct BcBlock {
    this_hash: Hash32,
    parent_hash: Hash32,
}
impl Default for BcBlock {
    fn default() -> Self {
        BcBlock {
            this_hash: Hash32::from_u64(0),
            parent_hash: Hash32::from_u64(0),
        }
    }
}
struct OnScreenBc {
    x: f32,
    y: f32,
    roundness: f32,
    darkness: f32,

    t_x: f32,
    t_y: f32,
    t_roundness: f32,
    t_darkness: f32,
    block: BcBlock,
}
impl Default for OnScreenBc {
    fn default() -> Self {
        OnScreenBc {
            x: 0.0,
            y: 0.0,
            roundness: 1.0,
            darkness: 0.0,
            t_x: 0.0,
            t_y: 0.0,
            t_roundness: 1.0,
            t_darkness: 0.0,
            block: BcBlock::default(),
        }
    }
}

pub struct VizState {
    camera_x: f32,
    camera_y: f32,
    zoom: f32,
    on_screen_bcs: HashMap<Hash32, OnScreenBc>,
    send_to_zebra: std::sync::mpsc::SyncSender<RequestToZebra>,
    receive_from_zebra: std::sync::mpsc::Receiver<ResponseFromZebra>,

    bc_tip_height: u64,
    bft_tip_height: u64,

    last_frame_hovered_hash: Hash32,
}
pub fn viz_gui_init() -> VizState {
    let (me_send, zebra_receive) = std::sync::mpsc::sync_channel(128);
    let (zebra_send, me_receive) = std::sync::mpsc::sync_channel(128);

    *REQUESTS_TO_ZEBRA.lock().unwrap() = Some(zebra_receive);
    *RESPONSES_FROM_ZEBRA.lock().unwrap() = Some(zebra_send);

    let mut viz_state = VizState {
        camera_x: 0.0,
        camera_y: 0.0,
        zoom: 0.0,
        on_screen_bcs: HashMap::new(),
        send_to_zebra: me_send,
        receive_from_zebra: me_receive,
        bc_tip_height: 0,
        bft_tip_height: 0,

        last_frame_hovered_hash: Hash32::from_u64(0),
    };
    let block = OnScreenBc { t_x: -10.0, t_y: -5.0, block: BcBlock { this_hash: Hash32::from_u64(1), parent_hash: Hash32::from_u64(0), }, ..Default::default() };
    viz_state.on_screen_bcs.insert(block.block.this_hash, block);
    let block = OnScreenBc { t_x: 10.0, t_y: -5.0, block: BcBlock { this_hash: Hash32::from_u64(2), parent_hash: Hash32::from_u64(1), }, ..Default::default() };
    viz_state.on_screen_bcs.insert(block.block.this_hash, block);
    let block = OnScreenBc { t_x: -10.0, t_y: -15.0, block: BcBlock { this_hash: Hash32::from_u64(3), parent_hash: Hash32::from_u64(2), }, ..Default::default() };
    viz_state.on_screen_bcs.insert(block.block.this_hash, block);
    viz_state
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

    // animations
    const MARGIN : f32 = 0.001;
    for on_screen_bc in viz_state.on_screen_bcs.values() {
        anything_happened |= (on_screen_bc.t_x - on_screen_bc.x).abs() > MARGIN;
        anything_happened |= (on_screen_bc.t_y - on_screen_bc.y).abs() > MARGIN;
        anything_happened |= (on_screen_bc.t_roundness - on_screen_bc.roundness).abs() > MARGIN;
        anything_happened |= (on_screen_bc.t_darkness - on_screen_bc.darkness).abs() > MARGIN;
    }

    anything_happened
}

fn e_lerp(from: f32, to: f32, dt: f32) -> f32 {
    let k = 15.0; // damping/smoothing rate; higher = faster response
    let delta = (to - from) * (1.0 - (-k * dt).exp());
    let next = from + delta;
    // Snap if we’re about to overshoot
    if (to - from).signum() != (to - next).signum() {
        to
    } else {
        next
    }
}

pub(crate) fn viz_gui_draw_the_stuff_for_the_things(viz_state: &mut VizState, draw_ctx: &DrawCtx, dt: f32, input_ctx: &InputCtx) {
    const ZOOM_FACTOR : f32 = 1.2;
    const SCREEN_UNIT_CONST : f32 = 10.0;
    {
        let dxm = (input_ctx.mouse_pos().0.clamp(0, draw_ctx.window_width) - draw_ctx.window_width/2) as f32;
        let dym = (input_ctx.mouse_pos().1.clamp(0, draw_ctx.window_height) - draw_ctx.window_height/2) as f32;
        let old_screen_unit = SCREEN_UNIT_CONST * ZOOM_FACTOR.powf(viz_state.zoom);
        viz_state.zoom += input_ctx.scroll_delta.1 as f32;
        let new_screen_unit = SCREEN_UNIT_CONST * ZOOM_FACTOR.powf(viz_state.zoom);
        viz_state.camera_x += (dxm / old_screen_unit) - (dxm / new_screen_unit);
        viz_state.camera_y += (dym / old_screen_unit) - (dym / new_screen_unit);
    }
    
    let zoom = ZOOM_FACTOR.powf(viz_state.zoom);
    // origin
    let screen_unit = SCREEN_UNIT_CONST * zoom;
    
    if input_ctx.mouse_held(MouseButton::Left) {
        viz_state.camera_x -= input_ctx.mouse_delta().0 as f32 / screen_unit;
        viz_state.camera_y -= input_ctx.mouse_delta().1 as f32 / screen_unit;
    }

    let origin_x = (draw_ctx.window_width / 2) as f32 - viz_state.camera_x * screen_unit;
    let origin_y = (draw_ctx.window_height / 2) as f32 - viz_state.camera_y * screen_unit;
    

    let world_mouse_x = viz_state.camera_x + ((input_ctx.mouse_pos().0.clamp(0, draw_ctx.window_width) - draw_ctx.window_width/2) as f32) / screen_unit;
    let world_mouse_y = viz_state.camera_y + ((input_ctx.mouse_pos().1.clamp(0, draw_ctx.window_height) - draw_ctx.window_height/2) as f32) / screen_unit;

    let mut hovered_block = Hash32::from_u64(0);
    for on_screen_bc in viz_state.on_screen_bcs.values() {
        let dx = on_screen_bc.x - world_mouse_x;
        let dy = on_screen_bc.y - world_mouse_y;
        if (dx*dx + dy*dy).sqrt() < 1.0 {
            hovered_block = on_screen_bc.block.this_hash;
        }
    }

    for on_screen_bc in viz_state.on_screen_bcs.values_mut() {
        if on_screen_bc.block.this_hash == hovered_block {
            on_screen_bc.t_roundness = 0.3;
            on_screen_bc.t_darkness = 0.2;
        } else {
            on_screen_bc.t_roundness = 1.0;
            on_screen_bc.t_darkness = 0.0;
        }
    }


    // animate to targets
    for on_screen_bc in viz_state.on_screen_bcs.values_mut() {
        on_screen_bc.x = e_lerp(on_screen_bc.x, on_screen_bc.t_x, dt);
        on_screen_bc.y = e_lerp(on_screen_bc.y, on_screen_bc.t_y, dt);
        on_screen_bc.roundness = e_lerp(on_screen_bc.roundness, on_screen_bc.t_roundness, dt);
        on_screen_bc.darkness = e_lerp(on_screen_bc.darkness, on_screen_bc.t_darkness, dt);
    }

    draw_ctx.circle(origin_x as f32, origin_y as f32, (screen_unit/2.0) as f32, 0xff_0000bb);

    for on_screen_bc in viz_state.on_screen_bcs.values() {
        let x = on_screen_bc.x;
        let y = on_screen_bc.y;
        let color = 0xff_000000 | blend_u32(0x000000, 0xffffff, ((1.0 - on_screen_bc.darkness) * 255.0) as u32);
        draw_ctx.circle_square(origin_x + (x*screen_unit), origin_y + (y*screen_unit), screen_unit, screen_unit*on_screen_bc.roundness, color);
        draw_ctx.mono_text_line((origin_x + (x + 1.5)*screen_unit) as f32, (origin_y + (y - 0.5)*screen_unit) as f32, screen_unit as f32, &format!("{}", on_screen_bc.block.this_hash), color);

        if let Some(parent) = viz_state.on_screen_bcs.get(&on_screen_bc.block.parent_hash) {
            let px = parent.x;
            let py = parent.y;
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


    if viz_state.last_frame_hovered_hash != hovered_block {
        viz_state.last_frame_hovered_hash = hovered_block;
        if hovered_block != Hash32::from_u64(0)
        { play_sound(SOUND_UI_HOVER, 0.5, 1.0); }
    }
}

fn split_vector(mut x: f32, mut y: f32) -> (f32, f32, f32) {
    let len = f32::sqrt(x*x + y*y);
    if len < 0.0000001 { return (0.0, 0.0, 0.0); }
    (x/len, y/len, len)
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Hash32 {
    le_chunks: [u64; 4],
}
impl Hash32 {
    #[inline]
    fn as_bytes(&self) -> [u8; 32] {
        let mut out = [0u8; 32];
        let mut i = 0;
        for &chunk in &self.le_chunks {
            let le = chunk.to_le_bytes(); // linear memory bytes for a LE u64
            out[i..i + 8].copy_from_slice(&le);
            i += 8;
        }
        out
    }
    fn from_u64(u: u64) -> Hash32 {
        Hash32 { le_chunks: [u,0u64,0u64,0u64], }
    }
}

impl std::fmt::Display for Hash32 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for b in self.as_bytes().iter().rev() {
            write!(f, "{:02x}", b)?;
        }
        Ok(())
    }
}

impl std::fmt::Debug for Hash32 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}