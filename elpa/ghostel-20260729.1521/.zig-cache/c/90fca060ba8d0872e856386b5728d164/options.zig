pub const @"src.terminal.build_options.Options.Artifact" = enum (u1) {
    ghostty = 0,
    lib = 1,
};
pub const artifact: @"src.terminal.build_options.Options.Artifact" = .lib;
pub const c_abi: bool = false;
pub const oniguruma: bool = false;
pub const simd: bool = true;
pub const slow_runtime_safety: bool = false;
pub const kitty_graphics: bool = true;
pub const tmux_control_mode: bool = false;
pub const version_string: []const u8 = "0.1.0-dev";
pub const version_major: usize = 0;
pub const version_minor: usize = 1;
pub const version_patch: usize = 0;
pub const version_pre: ?[]const u8 = "dev";
pub const version_build: ?[]const u8 = null;
