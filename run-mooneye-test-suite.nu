#! /usr/bin/env nu

def run-file [boot_rom: path, rom: path] {file: path, result: string, output: string} {
    let data: string = try {
        ./target/release/yarsge-headless $boot_rom $rom
    } catch {
        return {file: $rom, result: 💥, output: "crashed" }
    }

    if $data == "3/5/8/13/21/34" {
        return {file: $rom, result: ✔️, output: $data}
    } else if $data == "66/66/66/66/66/66" {
        return {file: $rom, result: ❌, output: $data}
    } else {
        return {file: $rom, result: ⁉️, output: $data}
    }
}

def main [boot_rom: path, base_dir: path] {
    cargo build --release --bin yarsge-headless

    let acceptance = ls ...(glob --no-dir $"($base_dir)/acceptance/**/*.{gb}") | get name
    let emulator_only = ls ...(glob --no-dir $"($base_dir)/emulator-only/**/*.{gb}") | get name

    let files = $acceptance | append $emulator_only | sort --natural

    $files | par-each { run-file $boot_rom $in | update file { $in | path relative-to $base_dir } } | sort-by file --natural | to md --pretty
}
