#! /usr/bin/env nu

def run-file [boot_rom: path, rom: path, outpath: path] {file: path, result: string} {
    echo ./target/release/yarsge-headless $boot_rom $rom "--screenshot-path" $"($outpath)"
    let data: string = try {
        ./target/release/yarsge-headless $boot_rom $rom --screenshot-path $"($outpath)"
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

    let all = ls ...(glob --no-dir $"($base_dir | path expand)/**/*.{gb}") | select name

    let files = $all | sort --natural | insert screenshot { $in.name | path relative-to $base_dir | path parse | update extension { "png" } | path join }
    
    $files | each { run-file $boot_rom $in.name $"artifacts/mealybug/($in.screenshot)" }
}
