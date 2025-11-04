use std::{hash::Hash};

use super::*;

use winit::keyboard::KeyCode;
use winit::event::MouseButton;

pub fn demo_of_rendering_stuff_with_context_that_allocates_in_the_background(gui: &mut GuiCtx) -> bool {
    gui.tooltip("This button exits the application", 24);
    if gui.button("Exit", 10, 10, 32) {
        return true;
    }

    gui.tooltip("This button says hello", 24);
    if gui.button("Say Hello", 10, 56, 32) {
        println!("hello!");
    }

    gui.draw().circle(gui.input().mouse_x, gui.input().mouse_y, 10, GuiColor::rgb(255.0, 0.0, 0.0).into());
    return false;
}


#[derive(Debug, Default, Clone)]
pub struct GuiCtx {
    pub input: *const InputCtx,
    pub draw:  *const DrawCtx,
    pub delta: f64,
    pub style: GuiStyle,

    deferred_tooltip:      Option<(u64, u64)>, // tooltip id, element id
    deferred_tooltip_pos:  (isize, isize),
    deferred_tooltip_wait: f64,

    elements:      std::collections::HashMap<u64, GuiElement>,
    tooltip_stack: Vec<u64>,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct GuiColor {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}

impl GuiColor {
    pub fn rgb(r: f32, g: f32, b: f32) -> Self {
        Self { r, g, b, a: 1.0 }
    }

    pub fn rgba(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self { r, g, b, a }
    }
}

impl Into<u32> for GuiColor {
    fn into(self) -> u32 {
        ((self.a * 255.0) as u8 as u32) << 24 |
        ((self.r * 255.0) as u8 as u32) << 16 |
        ((self.g * 255.0) as u8 as u32) << 8 |
        ((self.b * 255.0) as u8 as u32)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct GuiStyle {
    pub background: GuiColor,
    pub foreground: GuiColor,
    pub accent:     GuiColor,
    pub border:     GuiColor,
    pub highlight:  GuiColor,
}

impl GuiStyle {
    pub fn dark() -> Self {
        Self {
            background: GuiColor::rgb(0.1, 0.1, 0.1),
            foreground: GuiColor::rgb(0.9, 0.9, 0.9),
            accent:     GuiColor::rgb(0.5, 0.5, 0.5),
            border:     GuiColor::rgb(0.3, 0.3, 0.3),
            highlight:  GuiColor::rgb(0.2, 0.2, 0.2),
        }
    }

    pub fn light() -> Self {
        Self {
            background: GuiColor::rgb(0.9, 0.9, 0.9),
            foreground: GuiColor::rgb(0.1, 0.1, 0.1),
            accent:     GuiColor::rgb(0.5, 0.5, 0.5),
            border:     GuiColor::rgb(0.7, 0.7, 0.7),
            highlight:  GuiColor::rgb(0.8, 0.8, 0.8),
        }
    }
}

#[derive(Debug, Default, Clone)]
struct GuiElement {
    id:     u64,
    hot:    bool,
    active: bool,
    bounds: Bounds, // (x1, y1, x2, y2)

    label:        String,
    label_height: isize,
}

impl GuiElement {
    pub fn point_within_bounds(&self, px: isize, py: isize) -> bool {
        return self.bounds.x1 <= px && px <= self.bounds.x2
            && self.bounds.y1 <= py && py <= self.bounds.y2;
    }
}

#[derive(Debug, Default, Clone)]
struct Bounds {
    x1: isize, y1: isize,
    x2: isize, y2: isize,
}

impl Bounds {
    pub fn new(x1: isize, y1: isize, x2: isize, y2: isize) -> Self {
        Self { x1, y1, x2, y2 }
    }

    pub fn width(&self) -> isize {
        (self.x2 - self.x1).max(0)
    }

    pub fn height(&self) -> isize {
        (self.y2 - self.y1).max(0)
    }
}

fn magic<'a, 'b, T>(mut_ref: &'a mut T) -> &'b mut T {
    let mut_ref = mut_ref as *mut T;
    return unsafe { &mut *mut_ref };
}

impl GuiCtx {
    pub fn new() -> Self {
        Self { ..Default::default() }
    }

    pub fn begin_frame(&mut self) {
        self.tooltip_stack.clear();
    }

    pub fn end_frame(&mut self) {
        if let Some((tt_id, el_id)) = self.deferred_tooltip  {
            let el = self.elements.get(&el_id).unwrap();
            let tt = self.elements.get(&tt_id).unwrap();

            self.deferred_tooltip_wait += self.delta;
            if el.active || el.point_within_bounds(self.input().mouse_x, self.input().mouse_y) {
                if self.deferred_tooltip_wait >= 0.5 {
                    let x = self.deferred_tooltip_pos.0;
                    let y = self.deferred_tooltip_pos.1;

                    self.draw().rectangle(x, y, x + tt.bounds.width(), y + tt.label_height, 0x00_00_00_00);
                    self.draw().text_line(x, y, tt.label_height, &tt.label, 0xFFFFFF);
                }
            }
            else {
                self.deferred_tooltip      = None;
                self.deferred_tooltip_wait = 0.0;
            }
        }
    }

    #[track_caller]
    pub fn tooltip(&mut self, label: &str, height: isize /* @todo style */) {
        let element_id = self.unique_id(std::panic::Location::caller(), Some(label));
        let Some(el) = self.elements.get_mut(&element_id).map(magic) else {
            let width = self.draw().measure_text_line(height, label);
            self.elements.insert(element_id, GuiElement {
                id:           element_id,
                label:        label.to_string(),
                label_height: height,
                bounds:       Bounds::new(0, 0, width, height),
                ..Default::default()
            });

            return;
        };

        self.tooltip_stack.push(element_id);
        self.check_if_element_is_hot_or_active(el);
    }

    #[track_caller]
    pub fn button(&mut self, label: &str, x: isize, y: isize, height: isize) -> bool {
        let element_id = self.unique_id(std::panic::Location::caller(), Some(label));
        let Some(el) = self.elements.get_mut(&element_id).map(magic) else {
            let width = self.draw().measure_text_line(height, label);
            self.elements.insert(element_id, GuiElement {
                id:           element_id,
                label:        label.to_string(),
                label_height: height,
                bounds:       Bounds::new(x, y, x + width + 14, y + height + 5), // @todo style
                ..Default::default()
            });

            return false;
        };

        self.check_if_element_is_hot_or_active(el);
        self.maybe_show_tooltip_for(el);

        let mut color   = self.style.background;
        let mut clicked = false;
        if el.hot && el.active {
            clicked   = true;
            el.hot    = false;
            el.active = false;
        }
        else if el.active {
            color = self.style.highlight;
        }

        self.draw().rounded_rectangle(el.bounds.x1, el.bounds.y1, el.bounds.x2, el.bounds.y2, 5, color.into());
        // let center_x = el.bounds.0 + el.bounds.2 / 2;
        // let center_y = el.bounds.1 + el.bounds.3 / 2;
        self.draw().text_line(el.bounds.x1 + 7, el.bounds.y1 + 2, height, &el.label, self.style.foreground.into());

        return clicked;
    }

    fn maybe_show_tooltip_for(&mut self, el: &GuiElement) {
        if el.active && let Some(id) = self.tooltip_stack.pop() {
            if self.deferred_tooltip.is_some() {
                return;
            }

            self.deferred_tooltip     = Some((id, el.id));
            self.deferred_tooltip_pos = (el.bounds.x1 + el.bounds.width() / 2, el.bounds.y1 + el.bounds.height());
        }
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

    fn draw(&self)  -> &DrawCtx  { unsafe { &*self.draw  } }
    fn input(&self) -> &InputCtx { unsafe { &*self.input } }

    fn check_if_element_is_hot_or_active(&self, element: &mut GuiElement) {
        let mx = self.input().mouse_x;
        let my = self.input().mouse_y;

        element.active = element.point_within_bounds(mx, my);
        if element.active {
            if let (true, _, _) = self.input().mouse_held(MouseButton::Left) {
                element.hot = true;
            } else {
                element.hot = false;
            }
        }
    }
}
