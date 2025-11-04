use std::{collections::VecDeque, hash::Hash};
use winit::{keyboard::KeyCode, event::MouseButton};

use super::*;

macro_rules! gui_flags {
    ($(.$field:ident: $value:expr),*) => {
        GuiElementFlags {
            $(
                $field: $value,
            )*
            ..Default::default()
        }
    };
}

pub fn demo_of_rendering_stuff_with_context_that_allocates_in_the_background(gui: &mut GuiCtx) -> bool {
    if gui.input().key_pressed(KeyCode::Tab) {
        gui.debug = !gui.debug;
    }

    let mut layout = GuiRect::new(0, 0, gui.draw().window_width, gui.draw().window_height);

    // LEFT PANEL
    {
        let mut panel = layout.cut_from_left(gui.draw().window_width  / 4).inset(25)
            .debug_draw(gui, Some(GuiColor::DEBUG_MAGENTA));

        let mut first = panel.cut_from_top(40)
            .debug_draw(gui, None);
        gui.tooltip("This button says hello", 24);
        if gui.button("Say Hello", 32, first.prepare(CutFrom::Left), gui_flags!()) {
            println!("Hello!");
        }

        let mut second = panel.cut_from_top(40)
            .debug_draw(gui, Some(GuiColor::DEBUG_BLUE));
        gui.tooltip("This button says goodbye", 24);
        if gui.button("Say Goodbye", 32, second.prepare(CutFrom::Left), gui_flags!()) {
            println!("Goodbye!");
        }

        gui.tooltip("Press tab to toggle this button!", 24);
        if gui.button("Quit (*tab*)", 32, panel.prepare(CutFrom::Left), gui_flags!(.disabled: !gui.debug)) {
            return true;
        }
    }

    // RIGHT PANEL
    {
        let mut panel = layout.cut_from_right(gui.draw().window_width / 4).inset(25)
            .debug_draw(gui, Some(GuiColor::DEBUG_MAGENTA));

        let mut toolbar = panel.cut_from_top(100).inset(25);
        gui.tooltip("This button says goodbyte", 24);
        if gui.button("Say Goodbyte", 32, toolbar.prepare(CutFrom::Right), gui_flags!()) {
            println!("goodbyte!");
        }
    }

    gui.draw().circle(gui.input().mouse_x, gui.input().mouse_y, 5, GuiColor::rgb(1.0, 0.0, 0.0).into());

    return false;
}

fn magic<'a, 'b, T>(mut_ref: &'a mut T) -> &'b mut T {
    let mut_ref = mut_ref as *mut T;
    return unsafe { &mut *mut_ref };
}

#[derive(Debug, Default, Clone)]
pub struct GuiCtx {
    pub input: *const InputCtx,
    pub draw:  *const DrawCtx,
    pub delta: f64,
    pub style: GuiStyle,

    pub debug: bool,
    deferred_tooltip:      Option<(u64, u64)>, // tooltip id, element id
    deferred_tooltip_pos:  (isize, isize),
    deferred_tooltip_wait: f64,

    elements:      std::collections::HashMap<u64, GuiElement>,
    tooltip_stack: Vec<u64>,
}

#[derive(Debug, Default, Clone)]
struct GuiElement {
    id:     u64,
    hot:    bool,
    active: bool,
    flags:  GuiElementFlags,
    rect:   GuiRect,

    label:        String,
    label_height: isize,
}

#[derive(Debug, Default, Copy, Clone)]
pub struct GuiElementFlags {
    hidden:    bool,
    disabled:  bool,
    resizable: bool,
    inline:    bool,
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
                    let mut x = self.deferred_tooltip_pos.0;
                    let mut y = self.deferred_tooltip_pos.1;

                    if x + tt.rect.width() > self.draw().window_width
                     { x = self.draw().window_width - tt.rect.width(); }
                    if y + tt.label_height > self.draw().window_height
                     { y = self.draw().window_height - tt.label_height; }

                    self.draw().set_scissor(x, y, x + tt.rect.width(), y + tt.label_height);
                    self.draw().rectangle(x, y, x + tt.rect.width(), y + tt.label_height, self.style.background.dim(0.5).into());
                    self.draw().text_line(x, y, tt.label_height, &tt.label, self.style.foreground.into());
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
                rect:         GuiRect::new(0, 0, width, height),
                ..Default::default()
            });

            self.tooltip_stack.push(element_id);
            return;
        };

        self.tooltip_stack.push(element_id);
        self.update_element_state(el, el.flags.clone());
    }

    #[track_caller]
    pub fn button(&mut self, label: &str, height: isize, cut: GuiCut, flags: GuiElementFlags) -> bool {
        let element_id = self.unique_id(std::panic::Location::caller(), Some(label));
        let Some(el) = self.elements.get_mut(&element_id).map(magic) else {
            self.elements.insert(element_id, GuiElement {
                id:           element_id,
                flags:        flags,
                label:        label.to_string(),
                label_height: height,

                ..Default::default()
            });

            return !(flags.hidden || flags.disabled);
        };

        let width = self.draw().measure_text_line(height, label);
        el.rect   = cut.make(width).cut_from_top(height);

        let clicked = self.update_element_state(el, flags);
        self.maybe_show_tooltip_for(el);

        let mut bg = self.style.background;
        let mut fg = self.style.foreground;
        if el.flags.disabled {
            bg = self.style.background.dim(0.5);
            fg = self.style.foreground.dim(0.5);
        }
        else if el.active {
            bg = self.style.highlight;
        }

        if !el.flags.hidden {
            self.draw().set_scissor(el.rect.x1, el.rect.y1, el.rect.x2, el.rect.y2);
            self.draw().rectangle(el.rect.x1, el.rect.y1, el.rect.x2, el.rect.y2, bg.into());
            self.draw().text_line(el.rect.x1, el.rect.y1, height, &el.label, fg.into());
        }

        return clicked;
    }

    fn maybe_show_tooltip_for(&mut self, el: &GuiElement) {
        if el.active && let Some(id) = self.tooltip_stack.pop() {
            if self.deferred_tooltip.is_some() {
                return;
            }

            self.deferred_tooltip     = Some((id, el.id));
            self.deferred_tooltip_pos = (el.rect.x1 + el.rect.width() / 2, el.rect.y1 + el.rect.height());
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

    fn update_element_state(&self, element: &mut GuiElement, new_flags: GuiElementFlags) -> bool {
        element.flags = new_flags;

        if element.flags.disabled {
            element.hot    = false;
            element.active = false;
            return false;
        }

        let mx = self.input().mouse_x;
        let my = self.input().mouse_y;

        element.active = element.point_within_bounds(mx, my);
        if element.active {
            if let (true, _, _) = self.input().mouse_pressed(MouseButton::Left) {
                element.hot = true;
            } else {
                element.hot = false;
            }
        }

        let mut clicked = false;
        if element.hot && element.active {
            clicked        = true;
            element.hot    = false;
            element.active = false;
        }

        return clicked;
    }
}

impl GuiElement {
    pub fn point_within_bounds(&self, px: isize, py: isize) -> bool {
        return px >= self.rect.x1 && px <= self.rect.x2
            && py >= self.rect.y1 && py <= self.rect.y2;
    }
}

#[derive(Debug, Default, Clone)]
pub struct GuiRect {
    pub x1: isize,
    pub y1: isize,
    pub x2: isize,
    pub y2: isize,
}

#[derive(Debug)]
pub struct GuiCut {
    rect: *mut GuiRect,
    from: CutFrom,
}

#[derive(Debug, Clone)]
pub enum CutFrom {
    Top,
    Bottom,
    Left,
    Right,
}

impl GuiCut {
    pub fn make(&self, amount: isize) -> GuiRect {
        match self.from {
            CutFrom::Top    => unsafe { (*self.rect).cut_from_top(amount) },
            CutFrom::Bottom => unsafe { (*self.rect).cut_from_bottom(amount) },
            CutFrom::Left   => unsafe { (*self.rect).cut_from_left(amount) },
            CutFrom::Right  => unsafe { (*self.rect).cut_from_right(amount) },
        }
    }
}

impl GuiRect {
    pub fn new(x1: isize, y1: isize, x2: isize, y2: isize) -> Self {
        Self { x1, y1, x2, y2 }
    }

    pub fn width(&self) -> isize {
        (self.x2 - self.x1).max(0)
    }

    pub fn height(&self) -> isize {
        (self.y2 - self.y1).max(0)
    }

    pub fn cut_from_left(&mut self, amount: isize) -> Self {
        let cut = Self::new(self.x1, self.y1,  self.x1 + amount, self.y2);
        self.x1 += amount;
        return cut;
    }

    pub fn cut_from_right(&mut self, amount: isize) -> Self {
        let cut = Self::new(self.x2 - amount, self.y1, self.x2, self.y2);
        self.x2 -= amount;
        return cut;
    }

    pub fn cut_from_top(&mut self, amount: isize) -> Self {
        let cut = Self::new(self.x1, self.y1, self.x2, self.y1 + amount);
        self.y1 += amount;
        return cut;
    }

    pub fn cut_from_bottom(&mut self, amount: isize) -> Self {
        let cut = Self::new(self.x1, self.y2 - amount, self.x2, self.y2);
        self.y2 -= amount;
        return cut;
    }

    pub fn inset(&self, amount: isize) -> Self {
        Self::new(self.x1 + amount, self.y1 + amount, self.x2 - amount, self.y2 - amount)
    }

    pub fn outset(&self, amount: isize) -> Self {
        Self::new(self.x1 - amount, self.y1 - amount, self.x2 + amount, self.y2 + amount)
    }

    pub fn prepare(&mut self, from: CutFrom) -> GuiCut {
        return GuiCut{ rect: self, from }
    }

    pub fn debug_draw(self, gui: &GuiCtx, color: Option<GuiColor>) -> Self {
        if gui.debug {
            gui.draw().rectangle(self.x1, self.y1, self.x2, self.y2, color.unwrap_or(GuiColor::DEBUG_RED).into());
        }
        self
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct GuiColor {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}

impl GuiColor {
    pub const DEBUG_RED:     Self = Self { r: 1.0, g: 0.0, b: 0.0, a: 1.0 };
    pub const DEBUG_GREEN:   Self = Self { r: 0.0, g: 1.0, b: 0.0, a: 1.0 };
    pub const DEBUG_BLUE:    Self = Self { r: 0.0, g: 0.0, b: 1.0, a: 1.0 };
    pub const DEBUG_YELLOW:  Self = Self { r: 1.0, g: 1.0, b: 0.0, a: 1.0 };
    pub const DEBUG_MAGENTA: Self = Self { r: 1.0, g: 0.0, b: 1.0, a: 1.0 };
    pub const DEBUG_CYAN:    Self = Self { r: 0.0, g: 1.0, b: 1.0, a: 1.0 };

    pub fn rgb(r: f32, g: f32, b: f32) -> Self {
        Self { r, g, b, a: 1.0 }
    }

    pub fn rgba(r: f32, g: f32, b: f32, a: f32) -> Self {
        Self { r, g, b, a }
    }

    pub fn dim(&self, factor: f32) -> Self {
        Self {
            r: self.r * factor,
            g: self.g * factor,
            b: self.b * factor,
            a: self.a,
        }
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
