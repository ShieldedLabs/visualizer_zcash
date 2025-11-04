use std::{hash::Hash, ptr};

use super::*;

use winit::keyboard::KeyCode;
use winit::event::MouseButton;

pub fn demo_of_rendering_stuff_with_context_that_allocates_in_the_background(gui: &mut GuiCtx) -> bool {
    if gui.button("Exit", 10, 10, 32) {
        return true;
    }

    // draw_rectangle(draw, draw.window_width/2, draw.window_height/2, draw.window_width/2 + 100, draw.window_height/2 + 100, 0x0);

    // let first_text = "This text is to the left.";
    // let second_text = "This text is to the right.";
    // let offset = draw_measure_text_line(draw, 16, first_text);
    // draw_text_line(draw, draw.window_width/2 + offset, draw.window_height/2, 16, first_text, 0xff0000);
    // draw_text_line(draw, draw.window_width/2, draw.window_height/2, 16, first_text, 0xff);

    return false;
}


#[derive(Debug, Default, Clone)]
struct GuiElement {
    label:  String,
    bounds: (isize, isize, isize, isize),

    hot:    bool,
    active: bool,
}

impl GuiElement {
    pub fn point_is_within(&self, px: isize, py: isize) -> bool {
        return self.bounds.0 <= px && px <= self.bounds.2 && self.bounds.1 <= py && py <= self.bounds.3;
    }
}

#[derive(Debug, Default, Clone)]
pub struct GuiCtx {
    pub input: *const InputCtx,
    pub draw:  *const DrawCtx,
    pub style: GuiStyle,

    elements: std::collections::HashMap<u64, GuiElement>,
}

fn magic<'a, 'b, T>(mut_ref: &'a mut T) -> &'b mut T {
    let mut_ref = mut_ref as *mut T;
    return unsafe { &mut *mut_ref };
}

#[derive(Debug, Default, Clone, Copy)]
pub struct GuiStyle {
}

impl GuiCtx {
    pub fn new() -> Self {
        Self { ..Default::default() }
    }

    #[track_caller]
    pub fn button(&mut self, label: &str, x: isize, y: isize, height: isize) -> bool {
        let element_id = self.unique_id(std::panic::Location::caller(), Some(label));
        let Some(el) = self.elements.get_mut(&element_id).map(magic) else {
            // let width = draw_measure_text_line(self.draw(), height, label);
            let width = 100;
            self.elements.insert(element_id, GuiElement {
                label:  label.to_string(),
                bounds: (x, y, x + width, y + height),
                ..Default::default()
            });

            return false;
        };

        self.check_if_element_is_hot_or_active(el);

        let color: u32;
        let mut clicked = false;
        if el.hot && el.active {
            color     = 0xFFFFFFFF; // WHITE
            clicked   = true;
            el.hot    = false;
            el.active = false;
        }
        else if el.active {
            color = 0x00_00_FF_00; // GREEN
        }
        else if el.hot {
            color = 0x00_FF_00_00; // BLUE
        }
        else {
            color = 0x00_00_00_FF; // RED
        }
        draw_rectangle(self.draw(), el.bounds.0, el.bounds.1, el.bounds.2, el.bounds.3, color);

        let center_x = el.bounds.0 + el.bounds.2 / 2;
        let center_y = el.bounds.1 + el.bounds.3 / 2;
        draw_text_line(self.draw(), center_x, center_y, height, &el.label, 0x000000);

        return clicked;
    }

    fn unique_id(&self, caller: &std::panic::Location, label: Option<&str>) -> u64 {
        let mut hasher = std::hash::DefaultHasher::new();
        caller.file().hash(&mut hasher);
        caller.line().hash(&mut hasher);
        caller.column().hash(&mut hasher);

        if let Some(label) = label {
            label.hash(&mut hasher);
        }

        return hasher.finish();
    }

    fn draw(&self)  -> &DrawCtx  { unsafe { &*self.draw } }
    fn input(&self) -> &InputCtx { unsafe { &*self.input } }

    fn check_if_element_is_hot_or_active(&self, element: &mut GuiElement) {
        let mx = self.input().mouse_x;
        let my = self.input().mouse_y;
        element.active = element.point_is_within(mx, my);
        if element.active {
            if let (true, _, _) = self.input().mouse_held(MouseButton::Left) {
                element.hot = true;
            } else {
                element.hot = false;
            }
        }
    }
}
