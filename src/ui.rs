use std::{hash::Hash};
use winit::{keyboard::KeyCode, event::MouseButton};

use super::*;

// make widgets nicer to construct
macro_rules! widget {
    // Base case: only id and flags, no fields
    ($id:expr, $flags:expr) => {{
        Widget {
            id: $id,
            flags: $flags,
            ..Default::default()
        }
    }};

    // Case with fields: id, flags, then key-value pairs
    ($id:expr, $flags:expr, $($field:ident: $value:expr),* $(,)?) => {{
        Widget {
            id: $id,
            flags: $flags,
            $($field: $value,)*
            ..Default::default()
        }
    }};
}

// poor man's bitset
macro_rules! bitset {
    // bitset with no type, defaults to u64
    ($name:ident, $($flag:ident = $value:expr),* $(,)?) => {
        bitset!($name<u64>, $($flag = $value),*);
    };

    // bitset with provided type
    ($name:ident<$type:ty>, $($flag:ident = $value:expr),* $(,)?) => {
        #[derive(Debug, Default, Copy, Clone, PartialEq, Eq)]
        pub struct $name($type);

        #[allow(dead_code)]
        impl $name {
            $(pub const $flag: Self = Self($value);)*

            pub const fn contains(&self, flag: Self) -> bool {
                self.0 & flag.0 != 0
            }
        }

        impl Into<$type> for $name {
            fn into(self) -> $type {
                self.0
            }
        }

        impl From<$type> for $name {
            fn from(value: $type) -> Self {
                Self(value)
            }
        }

        impl std::ops::BitOr for $name {
            type Output = Self;
            fn bitor(self, rhs: Self) -> Self::Output {
                Self(self.0 | rhs.0)
            }
        }
        impl std::ops::BitOrAssign for $name {
            fn bitor_assign(&mut self, rhs: Self) {
                self.0 |= rhs.0;
            }
        }

        impl std::ops::BitAnd for $name {
            type Output = Self;
            fn bitand(self, rhs: Self) -> Self::Output {
                Self(self.0 & rhs.0)
            }
        }
        impl std::ops::BitAndAssign for $name {
            fn bitand_assign(&mut self, rhs: Self) {
                self.0 &= rhs.0;
            }
        }

        impl std::ops::Not for $name {
            type Output = Self;
            fn not(self) -> Self::Output {
                Self(!self.0)
            }
        }
    };
}

pub fn magic<'a, 'b, T>(mut_ref: &'a mut T) -> &'b mut T {
    let mut_ref = mut_ref as *mut T;
    return unsafe { &mut *mut_ref };
}


#[derive(Debug, Default)]
pub struct SomeDataToKeepAround {
    pub messages:          Vec<String>,
    pub can_send_messages: bool,
}

pub fn demo_of_rendering_stuff_with_context_that_allocates_in_the_background(ui: &mut Context, data: &mut SomeDataToKeepAround) -> bool {
    ui.begin_frame();

    if ui.input().key_pressed(KeyCode::Tab) {
        ui.debug = !ui.debug;
    }

    // !!!!!!!!!!!!!
    // @NOTE(JUDAH): there's a size calculation bug that's causing the layout to break
    // I shall fix it when I wake up
    // !!!!!!!!!!!!!

    /*
    let layout  = Rect::new(0f32, 0f32, ui.draw().window_width as f32, ui.draw().window_height as f32);
    let panel_w = layout.width() * 0.25;

    let left_panel   = ui.container(Rect::new(0.0, 0.0, panel_w, layout.height()).inset(10.0), Flags::RESIZABLE_X);
    let right_panel  = ui.container(Rect::new(layout.width() - panel_w, 0.0, panel_w, layout.height()).inset(10.0), Flags::RESIZABLE_X);
    let center_panel = ui.container(Rect::new(panel_w, 0.0, layout.width() - 2.0 * panel_w, layout.height()).inset(10.0), Flags::NONE);

    ui.push_parent(left_panel);
    {
        if ui.button("First!", Flags::NONE) {
            println!("pressed first!");
        }

        if ui.button("Second", Flags::NONE) {
            println!("pressed second!");
        }

        if ui.button("Third", Flags::NONE) {
            println!("pressed third!");
        }
    }
    ui.pop_parent(left_panel);


    ui.push_parent(center_panel);
    {
        // @todo: this is just going to get sent back, so the BFT tree knows where to draw
        ui.label("Center Panel", Flags::NONE);
    }
    ui.pop_parent(center_panel);


    ui.push_parent(right_panel);
    {
        if ui.button("Disable Textbox", Flags::NONE) {
            data.can_send_messages = !data.can_send_messages;
        }

        let flags = if data.can_send_messages { Flags::NONE } else { Flags::DISABLED };
        if let (true, text) = ui.textbox("Type in me!", flags) {
            data.messages.push(text);
        }

        for (i, message) in data.messages.iter().enumerate() {
            ui.label(&format!("{}##{}", message, i), Flags::NONE);
        }
    }
    ui.pop_parent(right_panel);
    */

    ui.end_frame();

    let (mx, my) = ui.input().mouse_pos();
    ui.draw().circle(mx, my, 5, Color::rgb(1.0, 0.0, 0.0).into());

    return false;
}

impl Context {
    pub fn new(style: Style) -> Self {
        Self { style, ..Default::default() }
    }

    pub const DEFAULT_CONTAINER_FLAGS: Flags = Flags(Flags::CLIP_CHILDREN.0 | Flags::DRAW_BACKGROUND.0 | Flags::DRAW_BORDER.0);

    #[track_caller]
    pub fn container(&mut self, rect: Rect, flags: Flags) -> WidgetId {
        let widget = magic(self.make_or_get_widget(Self::DEFAULT_CONTAINER_FLAGS | flags, None, std::panic::Location::caller()));
        widget.size = (Size::Exact(rect.width()), Size::Exact(rect.height()));
        self.update_widget_events(widget);
        return widget.id;
    }

    pub fn push_parent(&mut self, id: WidgetId) {
        self.parents.push(id);
        self.parent_stack.push(id);
    }

    pub fn pop_parent(&mut self, id: WidgetId) {
        let pid = self.parent_stack.pop();
        debug_assert!(id == pid, "parent ids did not match between push/pop");
    }


    pub const DEFAULT_LABEL_FLAGS: Flags = Flags(Flags::DRAW_MONO_TEXT.0);

    #[track_caller]
    pub fn label(&mut self, label: &str, flags: Flags) {
        let widget = magic(self.make_or_get_widget(Self::DEFAULT_LABEL_FLAGS | flags, Some(label), std::panic::Location::caller()));
        widget.size = (Size::TextContent, Size::TextContent);
        self.update_widget_events(widget);
    }


    pub const DEFAULT_BUTTON_FLAGS: Flags = Flags(Flags::CLICKABLE.0 | Flags::DRAW_MONO_TEXT.0 | Flags::DRAW_BACKGROUND.0 | Flags::DRAW_BORDER.0);

    #[track_caller]
    #[inline(always)]
    pub fn button(&mut self, label: &str, flags: Flags) -> bool {
        return self.button_events(label, flags).clicked;
    }

    #[track_caller]
    pub fn button_events(&mut self, label: &str, flags: Flags) -> WidgetEvents {
        let widget = magic(self.make_or_get_widget(Self::DEFAULT_BUTTON_FLAGS | flags, Some(label), std::panic::Location::caller()));
        widget.size = (Size::TextContent, Size::TextContent);
        return self.update_widget_events(widget);
    }


    pub const DEFAULT_TEXTBOX_FLAGS: Flags = Flags(Flags::CLICKABLE.0 | Flags::TYPEABLE.0 | Flags::DRAW_MONO_TEXT.0 | Flags::DRAW_BACKGROUND.0 | Flags::DRAW_BORDER.0);

    #[track_caller]
    #[inline(always)]
    pub fn textbox(&mut self, label: &str, flags: Flags) -> (bool, String) {
        let e = self.textbox_events(label, flags);
        return (e.submitted, e.text);
    }

    #[track_caller]
    pub fn textbox_events(&mut self, label: &str, flags: Flags) -> WidgetEvents {
        let widget = magic(self.make_or_get_widget(Self::DEFAULT_TEXTBOX_FLAGS | flags, Some(label), std::panic::Location::caller()));
        widget.size = (Size::PercentOfParent { amount: 1.0, tolerance: 1.0 }, Size::TextContent);
        return self.update_widget_events(widget);
    }


    pub fn begin_frame(&mut self) {
        self.active_widget = WidgetId::INVALID;
        self.hot_widget    = WidgetId::INVALID;

        self.parents.clear();
        self.clip_stack.reset();
        self.parent_stack.reset();

        self.draw_commands.clear();
    }

    pub fn end_frame(&mut self) {
        let total_parents = self.parents.len();
        for i in 0..total_parents {
            let widget = magic(self.widgets.get_mut(&self.parents[i]).unwrap());
            self.compute_widget_rects(widget);
        }
        for i in 0..total_parents {
            let widget = magic(self.widgets.get_mut(&self.parents[i]).unwrap());
            self.submit_widget_draw_commands(widget);
        }

        for cmd in &self.draw_commands {
            match cmd {
                DrawCommand::Rect(rect, Some(radius), color) => {
                    self.draw().rounded_rectangle(rect.x1 as isize, rect.y1 as isize, rect.x2 as isize, rect.y2 as isize, *radius as isize, (*color).into());
                },
                DrawCommand::Rect(rect, None, color) => {
                    self.draw().rectangle(rect.x1 as isize, rect.y1 as isize, rect.x2 as isize, rect.y2 as isize, (*color).into());
                },
                DrawCommand::Text(_, x, y, color, text) => {
                    self.draw().text_line(*x, *y, 24, text, (*color).into());
                },
                DrawCommand::Scissor(rect) => {
                    self.draw().set_scissor(rect.x1 as isize, rect.y1 as isize, rect.x2 as isize, rect.y2 as isize);
                }
            }
        }
    }
}

impl Context {
    fn update_widget_events(&mut self, widget: &mut Widget) -> WidgetEvents {
        let mut e = WidgetEvents::default();
        if widget.flags.contains(Flags::HIDDEN | Flags::DISABLED) {
            return e;
        }

        e.mouse = self.input().mouse_pos();

        if widget.flags.contains(Flags::CLICKABLE) {
            if widget.rel_rect.point_within(e.mouse.0 as f32, e.mouse.1 as f32) {
                self.active_widget = widget.id;
            }

            if self.active_widget == widget.id {
                if self.input().mouse_held(MouseButton::Left) {
                    e.pressed = true;
                    self.hot_widget = widget.id;
                }
            }

            if self.hot_widget == widget.id {
                if self.input().mouse_pressed(MouseButton::Left) {
                    e.clicked = true;
                }
                else if self.input().mouse_pressed(MouseButton::Right) {
                    e.right_clicked = true;
                }

                e.pressed = !(e.clicked || e.right_clicked);
            }
        }

        return e;
    }

    fn compute_widget_rects(&mut self, widget: &mut Widget) {
        if widget.flags.contains(Flags::HIDDEN) {
            return;
        }

        // compute x-axis
        match widget.size.0 {
            Size::Exact(pix) => {
                widget.abs_rect.x2 = pix;
            }
            Size::TextContent => {
                let text_width = self.draw().measure_text_line(24 /* @todo font */, &widget.display_text);
                widget.abs_rect.x2 = (text_width as f32 + self.style.padding) as f32;
            }

            Size::PercentOfParent { amount, tolerance } => {
                if let Some(parent_id) = widget.parent {
                    let parent = magic(self.widgets.get_mut(&parent_id).unwrap());
                    widget.abs_rect.x2 = parent.abs_rect.width() * amount;
                }
            }

            Size::SumOfChildren { tolerance } => {
                // post-order compute all child rects before calculating our size
                let mut computed_width = 0f32;
                for child in &widget.children {
                    let child = magic(self.widgets.get_mut(child).unwrap());
                    self.compute_widget_rects(child);
                    computed_width += child.abs_rect.width();
                }

                if computed_width > widget.abs_rect.width() {
                    let must_shrink_by = (computed_width - widget.abs_rect.width()) * tolerance / 100.0;
                    computed_width -= must_shrink_by;
                }

                widget.abs_rect.x2 = widget.abs_rect.x1 + computed_width;
            }

            _ => {}
        }

        // compute y-axis
        match widget.size.1 {
            Size::Exact(pix) => {
                widget.abs_rect.y2 = pix;
            }
            Size::TextContent => {
                widget.abs_rect.y2 = (24f32 /* @todo font */ + self.style.padding) as f32;
            }

            Size::PercentOfParent { amount, tolerance } => {
                if let Some(parent_id) = widget.parent {
                    let parent = magic(self.widgets.get_mut(&parent_id).unwrap());
                    widget.abs_rect.y2 = widget.abs_rect.y1 + (parent.abs_rect.height() * amount);
                }
            }

            Size::SumOfChildren { tolerance } => {
                // post-order compute all child rects before calculating our size
                let mut computed_height = 0f32;
                for child in &widget.children {
                    let child = magic(self.widgets.get_mut(child).unwrap());
                    self.compute_widget_rects(child);
                    computed_height += child.abs_rect.height();
                }

                if computed_height > widget.abs_rect.height() {
                    let must_shrink_by = (computed_height - widget.abs_rect.height()) * tolerance / 100.0;
                    computed_height   -= must_shrink_by;
                }

                widget.abs_rect.y2 = computed_height;
            }

            _ => {}
        }

        let should_compute_children = matches!(widget.size.0, Size::Exact(_) | Size::TextContent)
                                    | matches!(widget.size.1, Size::Exact(_) | Size::TextContent);
        if should_compute_children {
            for child in &widget.children {
                let child = magic(self.widgets.get_mut(child).unwrap());
                self.compute_widget_rects(child);
            }
        }

        if let Some(parent_id) = widget.parent {
            let parent = magic(self.widgets.get_mut(&parent_id).unwrap());
            if parent.rel_row.width() < widget.abs_rect.width() + self.style.spacing {
                parent.rel_row = parent.rel_rect.cut_from_top(widget.abs_rect.height()); // @todo layout
            }

            widget.rel_rect    = parent.rel_row.cut_from_left(widget.abs_rect.width());
            parent.rel_row.x1 += self.style.spacing;
        }
        else {
            widget.rel_rect = widget.abs_rect;
        }
    }

    fn submit_widget_draw_commands(&mut self, widget: &mut Widget) {
        if widget.flags.contains(Flags::HIDDEN) {
            return;
        }

        if widget.flags.contains(Flags::DRAW_BORDER) {
            self.draw_commands.push(DrawCommand::Rect(widget.rel_rect.outset(2f32), None, self.style.border));
        }

        if widget.flags.contains(Flags::DRAW_BACKGROUND) {
            let color = if self.active_widget == widget.id {
                self.style.accent
            } else {
                self.style.background
            };

            self.draw_commands.push(DrawCommand::Rect(widget.rel_rect, None, color));
        }

        if widget.flags.contains(Flags::DRAW_MONO_TEXT | Flags::DRAW_SERIF_TEXT) {
            let font = Font((widget.flags & Flags::DRAW_SERIF_TEXT).into()); // @todo font
            self.draw_commands.push(DrawCommand::Text(font, widget.rel_rect.x1 as isize, widget.rel_rect.y1 as isize, self.style.foreground, widget.display_text.to_string()));
        }

        for i in 0..widget.children.len() {
            let child_widget = magic(self.widgets.get_mut(&widget.children[i]).unwrap());
            self.submit_widget_draw_commands(child_widget);
        }
    }

    #[track_caller]
    fn get_parent_id(&self) -> WidgetId {
        return *self.parent_stack.peek();
    }

    fn get_parent(&mut self, id: WidgetId) -> &mut Widget {
        self.widgets.get_mut(&id).expect("parent widget id did not exist!")
    }

    fn make_or_get_widget(&mut self, flags: Flags, label: Option<&str>, loc: &std::panic::Location) -> &mut Widget {
        let mut h = xxhash3_64::Hasher::new();
        loc.file().hash(&mut h);
        loc.line().hash(&mut h);
        loc.column().hash(&mut h);

        let mut display_text = String::new();
        if let Some(label) = label {
            label.hash(&mut h);

            if let Some(idx) = label.find("##") {
                display_text = label[..idx].to_string();
            }
            else {
                display_text = label.to_string();
            }
        }

        let id = WidgetId(h.finish());
        let parent_id = if !self.parent_stack.empty() {
            let parent_id = self.get_parent_id();
            let parent    = self.get_parent(parent_id);
            parent.children.push(id);
            Some(parent_id)
        } else {
            None
        };

        let widget = self.widgets.entry(id).or_insert_with(|| widget!(id, flags,
            parent:       parent_id,
            display_text: display_text,
        ));

        widget.children.clear();

        return widget;
    }

    fn draw(&self)  -> &DrawCtx  { unsafe { &*self.draw  } }
    fn input(&self) -> &InputCtx { unsafe { &*self.input } }
}


#[derive(Debug, Default, Clone)]
pub struct Context {
    pub input: *const InputCtx,
    pub draw:  *const DrawCtx,
    pub debug: bool,

    pub delta: f64,
    pub style: Style,
    pub draw_commands: Vec<DrawCommand>,

    hot_widget:    WidgetId, // (focused, clicked)
    active_widget: WidgetId, // (hovered)

    widgets: std::collections::HashMap<WidgetId, Widget>,
    parents: std::vec::Vec<WidgetId>,

    clip_stack:   Stack<Rect>,
    parent_stack: Stack<WidgetId>,
}

bitset!(Flags<u64>,
    NONE = 0 << 0,

    HIDDEN   = 1 << 1,
    DISABLED = 1 << 2,

    CLICKABLE   = 1 << 9,
    TYPEABLE    = 1 << 10,
    RESIZABLE_X = 1 << 11,
    RESIZABLE_Y = 1 << 12,

    DRAW_MONO_TEXT  = 1 << 17,
    DRAW_SERIF_TEXT = 1 << 18,
    DRAW_BACKGROUND = 1 << 19,
    DRAW_BORDER     = 1 << 20,

    CLIP_CHILDREN = 1 << 25,
);

#[derive(Debug, Default, Copy, Clone, Eq, Hash, PartialEq, PartialOrd)]
pub struct WidgetId(u64);

impl WidgetId {
    pub const INVALID: WidgetId = WidgetId(0);
}

#[derive(Debug, Default, Copy, Clone)]
enum Size {
    #[default]
    None,
    Exact(f32),
    TextContent,
    PercentOfParent {
        amount:    f32,
        tolerance: f32,
    },
    SumOfChildren {
        tolerance: f32,
    },
}

#[derive(Debug, Default, Clone)]
struct Widget {
    id:    WidgetId,
    flags: Flags,

    parent:   Option<WidgetId>,
    children: Vec<WidgetId>,

    size: (Size, Size), // semantic size (x-axis, y-axis)

    // CALCULATED PER FRAME
    abs_rect: Rect, // absolute rect
    rel_rect: Rect, // parent-relative rect
    rel_row:  Rect, // current row cut from rel_rect

    // CONTROL FIELDS
    display_text: String,

    text_idx: usize,
    text_buf: String,

    slider_value:     f32,
    slider_value_min: f32,
    slider_value_max: f32,

    checked: bool,
}

#[derive(Debug, Default, Clone)]
pub struct WidgetEvents {
    mouse:      (isize, isize),
    drag_delta: (isize, isize),

    clicked:        bool,
    right_clicked:  bool,

    pressed:  bool,
    released: bool,
    dragging: bool,
    hovering: bool,

    text:      String,
    submitted: bool,
}

#[derive(Debug, Clone)]
pub enum DrawCommand {
    Scissor(Rect),
    Rect(Rect, Option<isize>, Color),
    Text(Font, isize, isize, Color, String),
}

#[derive(Debug, Default, Clone)]
pub struct Font(u64);

//////////////////////////////////////

#[derive(Debug, Default, Clone, Copy)]
pub struct Rect {
    pub x1: f32,
    pub y1: f32,
    pub x2: f32,
    pub y2: f32,
}

impl Rect {
    pub const ZERO: Self = Self { x1: 0f32, y1: 0f32, x2: 0f32, y2: 0f32 };

    pub fn new(x1: f32, y1: f32, x2: f32, y2: f32) -> Self {
        Self { x1, y1, x2, y2 }
    }

    pub fn width(&self) -> f32 {
        (self.x2 - self.x1).max(0f32)
    }

    pub fn height(&self) -> f32 {
        (self.y2 - self.y1).max(0f32)
    }

    pub fn point_within(&self, px: f32, py: f32) -> bool {
        return px >= self.x1 && px <= self.x2
            && py >= self.y1 && py <= self.y2;
    }

    pub fn cut_from_left(&mut self, a: f32) -> Self {
        let minx = self.x1;
        self.x1  = self.x2.min(self.x1 + a);
        return Self::new(minx, self.y1, self.x1, self.y2);
    }

    pub fn cut_from_right(&mut self, a: f32) -> Self {
        let maxx = self.x2;
        self.x2  = self.x1.max(self.x2 - a);
        return Self::new(self.x2, self.y1, maxx, self.y2);
    }

    pub fn cut_from_top(&mut self, a: f32) -> Self {
        let miny = self.y1;
        self.y1  = self.y2.min(self.y1 + a);
        return Self::new(self.x1, miny, self.x2, self.y1);
    }

    pub fn cut_from_bottom(&mut self, a: f32) -> Self {
        let maxy = self.y2;
        self.y2  = self.y1.max(self.y2 - a);
        return Self::new(self.x1, self.y2, self.x2, maxy);
    }

    pub fn inset(&self, amount: f32) -> Self {
        Self::new(self.x1 + amount, self.y1 + amount, self.x2 - amount, self.y2 - amount)
    }

    pub fn outset(&self, amount: f32) -> Self {
        Self::new(self.x1 - amount, self.y1 - amount, self.x2 + amount, self.y2 + amount)
    }

    pub fn prepare(&mut self, from: CutFrom) -> Cut {
        return Cut{ rect: self, from }
    }

    pub fn intersect(&self, other: Self) -> Self {
        let x1 = self.x1.max(other.x1);
        let y1 = self.y1.max(other.y1);
        let x2 = self.x2.min(other.x2);
        let y2 = self.y2.min(other.y2);
        Self::new(x1, y1, x2, y2)
    }
}

#[derive(Debug)]
pub struct Cut {
    rect: *mut Rect,
    from: CutFrom,
}

#[derive(Debug, Clone)]
pub enum CutFrom {
    Top,
    Bottom,
    Left,
    Right,
}

impl Cut {
    pub fn make(&self, amount: f32) -> Rect {
        match self.from {
            CutFrom::Top    => unsafe { (*self.rect).cut_from_top(amount)    },
            CutFrom::Bottom => unsafe { (*self.rect).cut_from_bottom(amount) },
            CutFrom::Left   => unsafe { (*self.rect).cut_from_left(amount)   },
            CutFrom::Right  => unsafe { (*self.rect).cut_from_right(amount)  },
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Color {
    pub r: f32,
    pub g: f32,
    pub b: f32,
    pub a: f32,
}

impl Color {
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

impl Into<u32> for Color {
    fn into(self) -> u32 {
        ((self.a * 255.0) as u8 as u32) << 24 |
        ((self.r * 255.0) as u8 as u32) << 16 |
        ((self.g * 255.0) as u8 as u32) << 8 |
        ((self.b * 255.0) as u8 as u32)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct Style {
    pub background: Color,
    pub foreground: Color,
    pub accent:     Color,
    pub border:     Color,
    pub highlight:  Color,

    pub padding: f32,
    pub spacing: f32,
}

impl Style {
    pub fn dark() -> Self {
        Self {
            background: Color::rgb(0.1, 0.1, 0.1),
            foreground: Color::rgb(0.9, 0.9, 0.9),
            accent:     Color::rgb(0.5, 0.5, 0.5),
            border:     Color::rgb(0.3, 0.3, 0.3),
            highlight:  Color::rgb(0.2, 0.2, 0.2),

            padding: 4f32,
            spacing: 8f32,
        }
    }
}

#[derive(Debug, Default, Clone)]
struct Stack<T>(std::vec::Vec<T>);

impl<T> Stack<T> {
    fn new() -> Self {
        Self(std::vec::Vec::new())
    }

    fn push(&mut self, item: T) {
        return self.0.push(item);
    }

    fn empty(&self) -> bool {
        self.0.is_empty()
    }

    #[track_caller]
    fn pop(&mut self) -> T {
        return self.0.pop().expect("stack was empty!");
    }

    #[track_caller]
    fn peek(&self) -> &T {
        return self.0.last().expect("stack was empty!");
    }

    fn peek_mut(&mut self) -> &mut T {
        return self.0.last_mut().expect("stack was empty!");
    }

    fn reset(&mut self) {
        self.0.clear();
    }
}
