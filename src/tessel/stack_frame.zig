pub const StackFrame = @This();

stack_frames: std.ArrayListUnmanaged(Frame),
frame_ptr: u32,

pub const StackFrameError = error{PoppingEmptyStackFrame};
const Error = StackFrameError || Allocator.Error;

pub const Frame = struct {
    ins: []const u8,
    ins_ptr: u32 = 0,
    stack_start: u32 = 0,
    num_locals: u32 = 0,
};

pub fn init() StackFrame {
    return .{
        .stack_frames = .{},
        .frame_ptr = 0,
    };
}

pub fn deinit(self: *StackFrame, allocator: Allocator) void {
    self.stack_frames.deinit(allocator);
}

pub fn push(
    self: *StackFrame,
    allocator: Allocator,
    instructions: []const u8,
    ins_ptr: u32,
    stack_start: u32,
    num_locals: u32,
) Allocator.Error!void {
    try self.stack_frames.append(allocator, Frame{
        .ins = instructions,
        .ins_ptr = ins_ptr,
        .stack_start = stack_start,
        .num_locals = num_locals,
    });
    self.frame_ptr += 1;
}

pub fn pop(self: *StackFrame) StackFrameError!void {
    if (self.frame_ptr == 0) {
        return StackFrameError.PoppingEmptyStackFrame;
    }
    self.frame_ptr -= 1;
    // self.stack_frames.shrinkRetainingCapacity(self.frame_ptr);
}

pub fn current_frame(self: *StackFrame) Frame {
    return self.stack_frames.items[self.frame_ptr - 1];
}

pub fn get_stack_ptr_at_depth(self: *StackFrame, depth: u32) u32 {
    std.debug.assert(depth < self.frame_ptr - 1);
    return self.stack_frames.items[self.frame_ptr - 1 - depth].stack_start;
}

pub fn get_num_locals_at_depth(self: *StackFrame, depth: u32) u32 {
    std.debug.assert(depth < self.frame_ptr - 1);
    return self.stack_frames.items[self.frame_ptr - 1 - depth].num_locals;
}

pub fn inc_current_frame_ins_ptr(self: *StackFrame, num: u32) void {
    std.debug.assert(self.frame_ptr > 0);
    self.stack_frames.items[self.frame_ptr - 1].ins_ptr += num;
}

pub fn set_current_frame_ins_ptr(self: *StackFrame, num: u32) void {
    std.debug.assert(self.frame_ptr > 0);
    self.stack_frames.items[self.frame_ptr - 1].ins_ptr = num;
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const testing = std.testing;
