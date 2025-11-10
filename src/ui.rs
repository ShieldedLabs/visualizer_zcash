use std::{hash::Hash};
use winit::{event::MouseButton, keyboard::KeyCode};

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

            pub const fn has(&self, flag: Self) -> bool {
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

    let mut layout = Rect::new(0f32, 0f32, ui.draw().window_width as f32, ui.draw().window_height as f32);
    let panel_w    = layout.width() * 0.25;
    let inset_amt  = 16.0;

    let left_panel   = ui.container_ex(layout.cut_from_left(panel_w).inset(inset_amt), Flags::DEFAULT_CONTAINER_FLAGS  | Flags::RESIZABLE_X);
    let right_panel  = ui.container_ex(layout.cut_from_right(panel_w).inset(inset_amt), Flags::DEFAULT_CONTAINER_FLAGS | Flags::RESIZABLE_X);

    let center_layout = layout.inset(inset_amt);
    let center_panel  = ui.container_ex(center_layout, Flags::DEFAULT_CONTAINER_FLAGS & !(Flags::DRAW_BACKGROUND | Flags::DRAW_BORDER));
    ui.non_ui_drawable_area = center_layout;

    ui.push_parent(left_panel);
    {
        // ui.layout(&[ Size::PercentOfParent{ amount: 1.0, tolerance: 0.0 } ]);
        if ui.button("One") {
            println!("pressed one!");
        }

        if ui.button("Two") {
            println!("pressed two!");
        }

        if ui.button("Three") {
            println!("pressed three!");
        }

        if ui.button("Four") {
            println!("pressed four!");
        }
    }
    ui.pop_parent(left_panel);

    ui.push_parent(center_panel);
    {
        // nothing for now
    }
    ui.pop_parent(center_panel);


    ui.push_parent(right_panel);
    {
        let event = ui.textbox_ex("Type here!", Flags::DEFAULT_TEXTBOX_FLAGS | if data.can_send_messages { Flags::KEEP_FOCUS } else { Flags::DISABLED });
        if event.submitted {
            let text = event.text.trim();
            if text.len() > 0 {
                data.messages.push(text.to_string());
            }
        }

        if ui.button("Clear Messages") {
            data.messages.clear();
        }

        if ui.button(if data.can_send_messages { "Disable Textbox" } else { "Enable Textbox" }) {
            data.can_send_messages = !data.can_send_messages;
        }

        if ui.checkbox("I am a checkbox?") {
            println!("box was checked!");
        }

        for (i, message) in data.messages.iter().enumerate() {
            ui.label(&format!("{}##{}", message, i));
        }
    }
    ui.pop_parent(right_panel);

    ui.end_frame();

    return false;
}

impl Context {
    pub fn new(style: Style) -> Self {
        Self { default_style: style, ..Default::default() }
    }

    #[track_caller]
    #[inline(always)]
    pub fn container(&mut self, rect: Rect) -> WidgetId {
        return self.container_ex(rect, Flags::DEFAULT_CONTAINER_FLAGS);
    }

    #[track_caller]
    pub fn container_ex(&mut self, rect: Rect, flags: Flags) -> WidgetId {
        let widget = magic(self.make_or_get_widget(flags, None, std::panic::Location::caller()));
        widget.size = (Size::Exact(rect.width()), Size::Exact(rect.height()));
        widget.abs_rect = rect;

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

    #[track_caller]
    #[inline(always)]
    pub fn label(&mut self, label: &str) {
        self.label_ex(label, Flags::DEFAULT_LABEL_FLAGS);
    }

    #[track_caller]
    pub fn label_ex(&mut self, label: &str, flags: Flags) -> WidgetEvents {
        let widget  = magic(self.make_or_get_widget(flags, Some(label), std::panic::Location::caller()));
        let font    = Font(0); // @todo font
        widget.size = (Size::TextContent(font), Size::TextContent(font));
        return self.update_widget_events(widget);
    }

    #[track_caller]
    #[inline(always)]
    pub fn button(&mut self, label: &str) -> bool {
        return self.button_ex(label, Flags::DEFAULT_BUTTON_FLAGS).clicked;
    }

    #[track_caller]
    pub fn button_ex(&mut self, label: &str, flags: Flags) -> WidgetEvents {
        let widget  = magic(self.make_or_get_widget(flags, Some(label), std::panic::Location::caller()));
        let font    = Font(0); // @todo font
        widget.size = (Size::TextContent(font), Size::TextContent(font));
        return self.update_widget_events(widget);
    }

    #[track_caller]
    #[inline(always)]
    pub fn checkbox(&mut self, label: &str) -> bool {
        return self.checkbox_ex(label, Flags::DEFAULT_CHECKBOX_FLAGS).clicked;
    }

    #[track_caller]
    pub fn checkbox_ex(&mut self, label: &str, flags: Flags) -> WidgetEvents {
        let widget  = magic(self.make_or_get_widget(flags, Some(label), std::panic::Location::caller()));
        let font    = Font(0); // @todo font
        widget.size = (Size::PercentOfParent { amount: 1.0, tolerance: 0.5 }, Size::TextContent(font));
        return self.update_widget_events(widget);
    }


    #[track_caller]
    #[inline(always)]
    pub fn textbox(&mut self, label: &str) -> (bool, String) {
        let e = self.textbox_ex(label, Flags::DEFAULT_TEXTBOX_FLAGS);
        return (e.submitted, e.text);
    }

    #[track_caller]
    pub fn textbox_ex(&mut self, label: &str, flags: Flags) -> WidgetEvents {
        let widget  = magic(self.make_or_get_widget(flags, Some(label), std::panic::Location::caller()));
        let font    = Font(0); // @todo font
        widget.size = (Size::PercentOfParent { amount: 1.0, tolerance: 1.0 }, Size::TextContent(font));
        return self.update_widget_events(widget);
    }


    pub fn push_style(&mut self, style: Style) {
        self.style_stack.push(style);
    }

    #[track_caller]
    pub fn pop_style(&mut self) {
        self.style_stack.pop();
    }

    pub fn get_style(&self) -> Style {
        if self.style_stack.empty() {
            return self.default_style;
        }
        else {
            return *self.style_stack.peek();
        }
    }


    pub fn begin_frame(&mut self) {
        self.active_widget = WidgetId::INVALID;
        self.hot_widget    = WidgetId::INVALID;

        self.parents.clear();
        self.style_stack.reset();
        self.clip_stack.reset();
        self.parent_stack.reset();

        self.draw_commands.clear();
    }

    pub fn end_frame(&mut self) {
        let total_parents = self.parents.len();

        // @todo speedup!!!!!!!!!

        // first pass, compute the exact size of widgets
        for i in 0..total_parents {
            let widget = magic(self.widgets.get_mut(&self.parents[i]).unwrap());
            self.compute_absolute_widget_rect(widget);
        }
        // second pass, compute widget sizes/positions relative to their parents
        for i in 0..total_parents {
            let widget = magic(self.widgets.get_mut(&self.parents[i]).unwrap());
            self.compute_relative_widget_rect(widget);
        }
        // third pass, we can finally render everything
        for i in 0..total_parents {
            let widget = magic(self.widgets.get_mut(&self.parents[i]).unwrap());
            self.push_widget_draw_commands(widget);
        }

        for cmd in &self.draw_commands {
            match cmd {
                DrawCommand::Rect(rect, Some(radius), color) => {
                    self.draw().rounded_rectangle(rect.x1 as isize, rect.y1 as isize, rect.x2 as isize, rect.y2 as isize, *radius as isize, (*color).into());
                }
                DrawCommand::Rect(rect, None, color) => {
                    self.draw().rectangle(rect.x1, rect.y1, rect.x2, rect.y2, (*color).into());
                }
                DrawCommand::Box(rect, thickness, color) => {
                    let x1 = rect.x1 as isize;
                    let y1 = rect.y1 as isize;
                    let x2 = rect.x2 as isize;
                    let y2 = rect.y2 as isize;
                    let t  = (*thickness / 2.0) as isize;

                    self.draw().rectangle((x1-t) as f32, (y1-t) as f32, (x1+t) as f32, (y2+t) as f32, (*color).into());
                    self.draw().rectangle((x2-t) as f32, (y1-t) as f32, (x2+t) as f32, (y2+t) as f32, (*color).into());
                    self.draw().rectangle((x1-t) as f32, (y1-t) as f32, (x2-t) as f32, (y1+t) as f32, (*color).into());
                    self.draw().rectangle((x1-t) as f32, (y2-t) as f32, (x2-t) as f32, (y2+t) as f32, (*color).into());
                }
                DrawCommand::Circle(x, y, radius, color) => {
                    self.draw().circle(*x as f32, *y as f32, *radius as f32, (*color).into());
                }

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
        if widget.flags.has(Flags::HIDDEN | Flags::DISABLED) {
            if self.active_widget == widget.id {
                self.active_widget = WidgetId::INVALID;
            }
            if self.hot_widget == widget.id {
                self.hot_widget = WidgetId::INVALID;
            }
            if self.hot_input == widget.id {
                self.hot_input = WidgetId::INVALID;
            }

            return e;
        }

        e.mouse = self.input().mouse_pos();

        if widget.flags.has(Flags::CLICKABLE) {
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

        if widget.flags.has(Flags::CHECKABLE) {
            if e.clicked {
                self.hot_input = widget.id;
                widget.checked = !widget.checked;
            }
        }

        if widget.flags.has(Flags::TYPEABLE) {
            if e.clicked {
                self.hot_input = widget.id;
            }

            if self.hot_input == widget.id {
                if let Some(text) = &self.input().text_input {
                    let before_idx   = &widget.text_buf[0..widget.text_idx];
                    let after_idx    = &widget.text_buf[widget.text_idx..];
                    let mut new_buf = Vec::new();
                    new_buf.extend_from_slice(before_idx);
                    new_buf.extend_from_slice(text);
                    new_buf.extend_from_slice(after_idx);
                    widget.text_buf = new_buf;
                    widget.text_idx += text.len();
                }

                if self.input().key_pressed(KeyCode::ArrowLeft) {
                    if widget.text_idx > 0 {
                        widget.text_idx -= 1;
                    }
                }
                if self.input().key_pressed(KeyCode::ArrowRight) {
                    if widget.text_idx < widget.text_buf.len() {
                        widget.text_idx += 1;
                    }
                }
                if self.input().key_pressed(KeyCode::Backspace) {
                    if widget.text_idx > 0 {
                        widget.text_buf.remove(widget.text_idx-1);
                        widget.text_idx -= 1;
                    }
                }
                if self.input().key_pressed(KeyCode::Delete) {
                    if widget.text_idx < widget.text_buf.len() {
                        widget.text_buf.remove(widget.text_idx);
                    }
                }

                if self.input().key_pressed(KeyCode::Enter) {
                    e.text      = widget.text_buf.iter().fold(String::new(), |s, c| format!("{}{}", s, c)); // Note(Sam): I will pray for forgiveness for this sin.
                    e.submitted = true;

                    if !widget.flags.has(Flags::KEEP_FOCUS) {
                        self.hot_input = WidgetId::INVALID;
                    }

                    widget.text_buf.clear();
                    widget.text_idx = 0;
                }

                if (self.input().key_pressed(KeyCode::Escape))
                || (self.input().mouse_pressed(MouseButton::Left) && self.hot_widget != widget.id)
                {
                    self.hot_input = WidgetId::INVALID;
                }
            }
        }

        if widget.flags.has(Flags::RESIZABLE_X) {
        }

        return e;
    }

    fn compute_absolute_widget_rect(&mut self, widget: &mut Widget) {
        if widget.flags.has(Flags::HIDDEN) {
            return;
        }

        let style = self.get_style();
        let mut computed_width  = 0f32;
        let mut computed_height = 0f32;

        // SumOfChildren is the only thing that requires computing children first
        let must_compute_children_first = matches!(widget.size, (Size::SumOfChildren { .. }, _) | (_, Size::SumOfChildren { .. }));
        if must_compute_children_first {
            for child in &widget.children {
                let child = magic(self.widgets.get_mut(child).unwrap());
                self.compute_absolute_widget_rect(child);
                computed_width  += child.abs_rect.width();
                computed_height += child.abs_rect.height();
            }
        }

        // x-axis size
        match widget.size.0 {
            Size::Exact(width_px) => {
                computed_width = width_px;
            }

            Size::TextContent(_font) => {
                let text_width = self.draw().measure_text_line(24 /* @todo font */, &widget.display_text);
                computed_width = (text_width as f32 + style.padding) as f32;
            }

            Size::PercentOfParent { amount: x_pct, tolerance: x_tol } => {
                if let Some(parent_id) = widget.parent {
                    let parent = magic(self.widgets.get_mut(&parent_id).unwrap());
                    computed_width = parent.abs_rect.width() * x_pct; // @todo tolerance
                }
            }

            Size::SumOfChildren { tolerance: x_tol } => {
            }

            _ => {}
        }

        // y-axis size
        match widget.size.1 {
            Size::Exact(height_px) => {
                computed_height = height_px;
            }
            Size::TextContent(_font) => {
                computed_height = (24f32 /* @todo font */ + style.padding) as f32;
            }

            Size::PercentOfParent { amount: y_pct, tolerance: y_tol } => {
                if let Some(parent_id) = widget.parent {
                    let parent = magic(self.widgets.get_mut(&parent_id).unwrap());
                    computed_height = parent.abs_rect.height() * y_pct; // @todo tolerance
                }
            }

            Size::SumOfChildren { tolerance: y_tol } => {
            }

            _ => {}
        }

        if !must_compute_children_first {
            for child in &widget.children {
                let child = magic(self.widgets.get_mut(child).unwrap());
                self.compute_absolute_widget_rect(child);
                computed_width  += child.abs_rect.width();
                computed_height += child.abs_rect.height();
            }
        }

        if widget.parent.is_some() {
            widget.abs_rect.x2 = computed_width;
            widget.abs_rect.y2 = computed_height;
        }
    }

    fn compute_relative_widget_rect(&mut self, widget: &mut Widget) {
        let style = self.get_style();

        if let Some(parent_id) = widget.parent {
            let parent = magic(self.widgets.get_mut(&parent_id).unwrap());
            if parent.rel_row.width() < widget.abs_rect.width() + style.spacing {
                parent.rel_row = parent.rel_rect.cut_from_top(widget.abs_rect.height()); // @todo layout
            }

            widget.rel_rect    = parent.rel_row.cut_from_left(widget.abs_rect.width()).cut_from_top(widget.abs_rect.height());
            parent.rel_row.x1 += style.spacing;
        }
        else {
            widget.rel_rect = widget.abs_rect;
            widget.rel_row  = widget.rel_rect;
        }

        for i in 0..widget.children.len() {
            let child = magic(self.widgets.get_mut(&widget.children[i]).unwrap());
            self.compute_relative_widget_rect(child);
        }
    }

    fn push_widget_draw_commands(&mut self, widget: &mut Widget) {
        if widget.flags.has(Flags::HIDDEN) {
            return;
        }

        let style = self.get_style();

        if widget.flags.has(Flags::DRAW_BACKGROUND) {
            let mut color = style.background;
            if widget.flags.has(Flags::CLICKABLE | Flags::TYPEABLE) {
                if self.active_widget == widget.id {
                    color = color.dim(2.0); // @todo style
                }
                if self.hot_widget == widget.id {
                    color = color.dim(0.1); // @todo style
                }
            }

            if widget.flags.has(Flags::DISABLED) {
                color = color.dim(0.5);
            }

            self.draw_commands.push(DrawCommand::Rect(widget.rel_rect, None, color));
        }

        if widget.flags.has(Flags::DRAW_PIP) {
            let color = style.foreground;
            self.draw_commands.push(DrawCommand::Circle(widget.rel_rect.x1 - 12.0, widget.rel_rect.y1 + (widget.rel_rect.height() / 2.0) - 1.0, 4.0, color)); // @todo style

            if widget.checked {
                self.draw_commands.push(DrawCommand::Circle(widget.rel_rect.x1 - 12.0, widget.rel_rect.y1 + (widget.rel_rect.height() / 2.0) - 1.0, 2.0, color.dim(0.25))); // @todo style
            }
        }

        if widget.flags.has(Flags::DRAW_MONO_TEXT | Flags::DRAW_SERIF_TEXT) {
            let mut text  = widget.display_text.to_string();
            let mut color = style.foreground;
            if widget.flags.has(Flags::TYPEABLE) {
                if widget.text_buf.len() > 0 {
                    text = widget.text_buf.iter().fold(String::new(), |s, c| format!("{}{}", s, c)); // Note(Sam): I will pray for forgiveness for this sin.
                }
                else {
                    color = style.foreground.dim(0.5); // @todo style
                }
            }

            let color = if widget.flags.has(Flags::DISABLED) { color.dim(0.5) } else { color }; // @todo style
            let font  = Font((widget.flags & Flags::DRAW_SERIF_TEXT).into()); // @todo font
            self.draw_commands.push(DrawCommand::Text(font, widget.rel_rect.x1 as isize, widget.rel_rect.y1 as isize, color, text));
        }

        if widget.flags.has(Flags::TYPEABLE) && self.hot_input == widget.id {
            let text_before_idx = widget.text_buf[0..widget.text_idx].iter().fold(String::new(), |s, c| format!("{}{}", s, c)); // Note(Sam): I will pray for forgiveness for this sin.

            let x1: f32;
            if widget.text_idx == 0 {
                x1 = widget.rel_rect.x1 + 1.0; // @todo style
            }
            else {
                let width_of_current_text = self.draw().measure_text_line(24 /* @todo font */, &text_before_idx) as f32;
                x1 = widget.rel_rect.x1 + width_of_current_text as f32;
            }

            let cursor_rect = Rect::new(x1, widget.rel_rect.y1 + 4.0, x1 + 1.0, widget.rel_rect.y2 - 4.0); // @todo style
            self.draw_commands.push(DrawCommand::Rect(cursor_rect, None, Color::DEBUG_RED)); // @todo style
        }

        if widget.flags.has(Flags::DRAW_BORDER) {
            self.draw_commands.push(DrawCommand::Box(widget.rel_rect, 2f32 /* @todo style */, style.border));
        }

        if self.debug {
            self.draw_commands.push(DrawCommand::Box(widget.rel_rect, 2f32 /* @todo style */, Color::DEBUG_MAGENTA.fade(0.25)));
        }

        for i in 0..widget.children.len() {
            let child = magic(self.widgets.get_mut(&widget.children[i]).unwrap());
            self.push_widget_draw_commands(child);
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

        widget.flags = flags;
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
    pub non_ui_drawable_area: Rect,

    pub delta: f64,
    pub default_style: Style,
    pub draw_commands: Vec<DrawCommand>,

    hot_widget:    WidgetId, // (focused, clicked)
    hot_input:     WidgetId, // (focused, clicked)
    active_widget: WidgetId, // (hovered)

    widgets: std::collections::HashMap<WidgetId, Widget>,
    parents: std::vec::Vec<WidgetId>,

    style_stack:  Stack<Style>,
    clip_stack:   Stack<Rect>,
    parent_stack: Stack<WidgetId>,
}

bitset!(Flags<u64>,
    NONE = 0 << 0,

    HIDDEN     = 1 << 1,
    DISABLED   = 1 << 2,

    // HOVERABLE   = 1 <<  9,
    CLICKABLE   = 1 << 10,
    CHECKABLE   = 1 << 11,
    TYPEABLE    = 1 << 12,
    RESIZABLE_X = 1 << 13,
    RESIZABLE_Y = 1 << 14,

    DRAW_MONO_TEXT  = 1 << 17,
    DRAW_SERIF_TEXT = 1 << 18,
    DRAW_BACKGROUND = 1 << 19,
    DRAW_BORDER     = 1 << 20,
    DRAW_PIP        = 1 << 21,

    CLIP_CHILDREN = 1 << 25,
    KEEP_FOCUS    = 1 << 26,


    // Widget Defaults
    //////////////////

    DEFAULT_LABEL_FLAGS = Flags::DRAW_MONO_TEXT.0,

    DEFAULT_BUTTON_FLAGS = Flags::CLICKABLE.0
                         | Flags::DRAW_MONO_TEXT.0
                         | Flags::DRAW_BACKGROUND.0
                         | Flags::DRAW_BORDER.0,

    DEFAULT_CHECKBOX_FLAGS = Flags::CHECKABLE.0
                           | Flags::CLICKABLE.0
                           | Flags::DRAW_MONO_TEXT.0
                           | Flags::DRAW_PIP.0,

    DEFAULT_TEXTBOX_FLAGS = Flags::TYPEABLE.0
                          | Flags::CLICKABLE.0
                          | Flags::DRAW_MONO_TEXT.0
                          | Flags::DRAW_BACKGROUND.0
                          | Flags::DRAW_BORDER.0,

    DEFAULT_CONTAINER_FLAGS = Self::DRAW_MONO_TEXT.0
                            | Self::DRAW_SERIF_TEXT.0
                            | Self::DRAW_BACKGROUND.0
                            | Self::DRAW_BORDER.0,
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
    TextContent(Font),
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
    size:     (Size, Size), // semantic size (x-axis, y-axis)

    // CALCULATED PER FRAME
    abs_rect: Rect, // absolute rect
    rel_rect: Rect, // parent-relative rect
    rel_row:  Rect, // current row cut from rel_rect

    // CONTROL FIELDS
    display_text: String,

    text_idx: usize,
    text_buf: Vec<char>,

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
    resizing: bool,
    hovering: bool,

    text:      String,
    submitted: bool,
}

#[derive(Debug, Clone)]
pub enum DrawCommand {
    Scissor(Rect),
    Rect(Rect, Option<isize>, Color),
    Circle(f32, f32, f32, Color),
    Box(Rect, f32, Color),
    Text(Font, isize, isize, Color, String),
}

#[derive(Debug, Default, Copy, Clone)]
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

    pub fn fade(&self, factor: f32) -> Self {
        Self {
            r: self.r,
            g: self.g,
            b: self.b,
            a: self.a * factor,
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
