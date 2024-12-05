const std = @import("std");

const day01 = @import("day01.zig");
const Problems = enum {day01};

const Error = error{
    MissingArgument,
    UnknownProblem,
};

pub fn main() !void {
    var args = std.process.args();
    var counter: usize = 0;
    var arg1: ?[]const u8 = null;
    var arg2: ?[]const u8 = null;
    while (args.next()) |arg| {
        if (counter == 1) {
            arg1= arg;
        }
        if (counter == 2) {
            arg2 = arg;
        }
        counter += 1;
    }

    const problem_name = arg1 orelse {
        std.log.err("Please supply the problem name as the first argument\n", .{});
        return Error.MissingArgument;
    };

    const input_file = arg2 orelse {
        std.log.err("Please supply the input file as the second argument\n", .{});
        return Error.MissingArgument;
    };


    const problem = std.meta.stringToEnum(Problems, problem_name) orelse {
        std.log.err("Unknown problem name: {s}\n", .{problem_name});
        return Error.UnknownProblem;
    };
    switch (problem) {
        Problems.day01 => {
            try day01.solve(input_file);
        }
    }
}
