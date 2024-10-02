let catppuccin = {
  mocha: {
    rosewater: "#f5e0dc"
    flamingo: "#f2cdcd"
    pink: "#f5c2e7"
    mauve: "#cba6f7"
    red: "#f38ba8"
    maroon: "#eba0ac"
    peach: "#fab387"
    yellow: "#f9e2af"
    green: "#a6e3a1"
    teal: "#94e2d5"
    sky: "#89dceb"
    sapphire: "#74c7ec"
    blue: "#89b4fa"
    lavender: "#b4befe"
    text: "#cdd6f4"
    subtext1: "#bac2de"
    subtext0: "#a6adc8"
    overlay2: "#9399b2"
    overlay1: "#7f849c"
    overlay0: "#6c7086"
    surface2: "#585b70"
    surface1: "#45475a"
    surface0: "#313244"
    base: "#1e1e2e"
    mantle: "#181825"
    crust: "#11111b"
  }
}


let stheme = $catppuccin.mocha
let theme = {
  separator: $stheme.overlay0
  leading_trailing_space_bg: $stheme.overlay0
  header: $stheme.green
  date: $stheme.mauve
  filesize: $stheme.blue
  row_index: $stheme.pink
  bool: $stheme.peach
  int: $stheme.peach
  duration: $stheme.peach
  range: $stheme.peach
  float: $stheme.peach
  string: $stheme.green
  nothing: $stheme.peach
  binary: $stheme.peach
  cellpath: $stheme.peach
  hints: dark_gray

  shape_garbage: { fg: $stheme.crust bg: $stheme.red attr: b }
  shape_bool: $stheme.blue
  shape_int: { fg: $stheme.mauve attr: b}
  shape_float: { fg: $stheme.mauve attr: b}
  shape_range: { fg: $stheme.yellow attr: b}
  shape_internalcall: { fg: $stheme.blue attr: b}
  shape_external: { fg: $stheme.blue attr: b}
  shape_externalarg: $stheme.text 
  shape_literal: $stheme.blue
  shape_operator: $stheme.yellow
  shape_signature: { fg: $stheme.green attr: b}
  shape_string: $stheme.green
  shape_filepath: $stheme.yellow
  shape_globpattern: { fg: $stheme.blue attr: b}
  shape_variable: $stheme.text
  shape_flag: { fg: $stheme.blue attr: b}
  shape_custom: {attr: b}
}

$env.config = {   
  show_banner: false
  # use_ansi_coloring: false
  render_right_prompt_on_last_line: true
  hooks: {
    command_not_found: {
      |cmd_name| (
        if ($nu.os-info.name == "linux" and 'CNF' in $env) {try {
          let raw_results = (nix-locate --minimal --no-group --type x --type s --top-level --whole-name --at-root $"/bin/($cmd_name)")
          let parsed = ($raw_results | split row "\n" | each {|elem| ($elem | parse "{attr}.{output}" | first) })
          let names = ($parsed | each {|row|
            if ($row.output == "out") {
              $row.attr
            } else {
              $"($row.attr).($row.output)"
            }
          })
          let names_display = ($names | str join "\n")
          (
            "nix-index found the follwing matches:\n\n" + $names_display
          )
        } catch {
          null
        }}
      )
    }
  }  
 completions: {
   quick: true
   partial: true
   algorithm: "fuzzy"
  }
}

def nixup [] { 
  cd ~/sysflake; ^nixos-rebuild switch --flake .#Tsu --use-remote-sudo --show-trace --print-build-logs 
}

alias nix-upd = nixup
