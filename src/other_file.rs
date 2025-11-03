
use super::*;

pub fn demo_of_rendering_stuff_with_context_that_allocates_in_the_background(draw_ctx: &DrawCtx) {
    
    draw_rectangle(draw_ctx, draw_ctx.window_width/2, draw_ctx.window_height/2, draw_ctx.window_width/2 + 100, draw_ctx.window_height/2 + 100, 0x0);

    let first_text = "This text is to the left.";
    let second_text = "This text is to the right.";
    let offset = draw_measure_text_line(draw_ctx, 16, first_text);
    draw_text_line(draw_ctx, draw_ctx.window_width/2 + offset, draw_ctx.window_height/2, 16, first_text, 0xff0000);
    draw_text_line(draw_ctx, draw_ctx.window_width/2, draw_ctx.window_height/2, 16, first_text, 0xff);

}