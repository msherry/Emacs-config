pub const flatpak: bool = false;
pub const snap: bool = false;
pub const x11: bool = false;
pub const wayland: bool = false;
pub const sentry: bool = true;
pub const simd: bool = true;
pub const i18n: bool = true;
pub const @"src.apprt.runtime.Runtime" = enum (u1) {
    none = 0,
    gtk = 1,
};
pub const app_runtime: @"src.apprt.runtime.Runtime" = .none;
pub const @"src.font.backend.Backend" = enum (u3) {
    freetype = 0,
    freetype_windows = 1,
    fontconfig_freetype = 2,
    coretext = 3,
    coretext_freetype = 4,
    coretext_harfbuzz = 5,
    coretext_noshape = 6,
    web_canvas = 7,
};
pub const font_backend: @"src.font.backend.Backend" = .coretext;
pub const @"src.renderer.backend.Backend" = enum (u2) {
    opengl = 0,
    metal = 1,
    webgl = 2,
};
pub const renderer: @"src.renderer.backend.Backend" = .metal;
pub const @"src.build.Config.ExeEntrypoint" = enum (u3) {
    ghostty = 0,
    helpgen = 1,
    mdgen_ghostty_1 = 2,
    mdgen_ghostty_5 = 3,
    webgen_config = 4,
    webgen_actions = 5,
    webgen_commands = 6,
};
pub const exe_entrypoint: @"src.build.Config.ExeEntrypoint" = .ghostty;
pub const @"src.os.wasm.target.Target" = enum (u0) {
    browser = 0,
};
pub const wasm_target: @"src.os.wasm.target.Target" = .browser;
pub const wasm_shared: bool = true;
pub const app_version: @import("std").SemanticVersion = .{
    .major = 1,
    .minor = 3,
    .patch = 2,
};
pub const app_version_string: [:0]const u8 = "1.3.2";
pub const lib_version: @import("std").SemanticVersion = .{
    .major = 0,
    .minor = 1,
    .patch = 0,
    .pre = "dev",
};
pub const lib_version_string: [:0]const u8 = "0.1.0-dev";
pub const @"src.build.Config.ReleaseChannel" = enum (u1) {
    tip = 0,
    stable = 1,
};
pub const release_channel: @"src.build.Config.ReleaseChannel" = .stable;
