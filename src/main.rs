use core::fmt;
use std::{collections::{HashMap, HashSet}, fmt::{Display, Formatter}, fs::{self, File}, ops::Add, path::{Path, PathBuf}};
use uuid::Uuid;
use std::time::{Duration, Instant};
use rust_dmx::{available_ports, DmxPort};
use serde::{Serialize, Deserialize};

use tui::{
    backend::{Backend, CrosstermBackend}, layout::{Constraint, Direction, Layout}, style::{Color as TUIColor, Modifier, Style}, symbols, text::{Span, Spans}, widgets::{Axis, Block, Borders, Chart, Dataset, List, ListItem, Paragraph}, Terminal
};
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};

use std::io;

#[derive(Clone, Copy, PartialEq, Serialize, Deserialize)]
struct Color{
    r:u8,
    g:u8,
    b:u8
}

impl Color{
    fn new(r:u8, g:u8, b:u8) -> Color{
        Color{r, g, b}
    }
}

#[derive(Clone, Copy, Serialize, Deserialize)]
struct Position3D{
    x:f32,
    y:f32,
    z:f32
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]

enum Transition{
    Instant,
    Interpolate{ease_in:bool, ease_out:bool},
}

impl fmt::Display for Transition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Transition::Instant => write!(f, "Instant"),
            Transition::Interpolate { ease_in, ease_out} => write!(
                f, 
                "Interpolate (ease_in: {}, ease_out: {})", 
                ease_in, 
                ease_out, 
            ),
        }
    }
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
enum AudioFrequencyBand{
    Low,
    Mid,
    High
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]

struct SignalSource {
    steps: u8, 
    pulses: u8, 
    shift: u8,
    transition: Transition
}

impl SignalSource {
    fn get_state_at(self, current_steps: f32) -> f32 {
        let total_steps = self.steps as f32;
        let completed_loops = (current_steps / total_steps).floor();
        let current_loop_progress = current_steps % total_steps;

        // Calculate pulses passed in completed loops
        let pulses_in_completed_loops = (self.pulses as u32 * completed_loops as u32) as u32;

        // Calculate pulses passed in the current loop
        let mut pulses_in_current_loop = 0;
        for i in 0..self.steps as u32 {
            if ((i * self.pulses as u32 + self.shift as u32) % self.steps as u32) < self.pulses as u32 {
                if i as f32 <= current_loop_progress {
                    pulses_in_current_loop += 1;
                }
            }
        }

        // Total pulses passed
        let total_pulses_passed = pulses_in_completed_loops + pulses_in_current_loop;

        // Determine if odd or even number of pulses have passed
        let odd = total_pulses_passed % 2 == 1;

        // Find the last elapsed pulse and next upcoming pulse
        let mut last_pulse = 0.0;
        let mut next_pulse = total_steps;
        for i in 0..self.steps as u32 {
            let step = ((i * self.pulses as u32 + self.shift as u32) % self.steps as u32) as u32;
            if step < self.pulses as u32 {
                let pulse_position = i as f32;
                if pulse_position <= current_loop_progress {
                    last_pulse = pulse_position;
                } else {
                    next_pulse = pulse_position;
                    break;
                }
            }
        }

        // Calculate alpha (progress between last and next pulse)
        let alpha = if next_pulse == last_pulse {
            0.0
        } else {
            (current_loop_progress - last_pulse) / (next_pulse - last_pulse)
        };

        match self.transition {
            Transition::Instant => {
                if odd { 1.0 } else { 0.0 }
            },
            Transition::Interpolate { ease_in, ease_out } => {
                let mut interpolated = if odd { 1.0 - alpha } else { alpha };
                
                if ease_in && ease_out {
                    // Smooth both in and out (sigmoid-like)
                    interpolated = (1.0 - (interpolated * std::f32::consts::PI).cos()) / 2.0;
                } else if ease_in {
                    // Smooth in
                    interpolated = 1.0 - (1.0 - interpolated).powi(2);
                } else if ease_out {
                    // Smooth out
                    interpolated = interpolated.powi(2);
                }

                interpolated
            }
        }
    }

    fn represent(&self, resolution: usize) -> Vec<f32> {
        let mut result = Vec::with_capacity(resolution);
        
        for i in 0..resolution {
            let current_steps = (i as f32 / resolution as f32) * self.steps as f32;
            let state = self.get_state_at(current_steps);
            result.push(state);
        }
        
        result
    }

}

#[derive(Clone, Copy, Serialize, Deserialize)]

struct ProjetorSourceWindow{
    position: (f32,f32),
    scale: f32,
    rotation: f32
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]

enum ProjectorVisual {
    Camera,
    BlackAndWhite
}

impl ProjectorVisual {
}

#[derive(Clone, Serialize, Deserialize)]
enum FixtureMode{
    Transparent,
    On{color:Color},
    ToggleOnSignal{a:Box<FixtureMode>, b:Box<FixtureMode>, signal_source:SignalSource},
    Projector(ProjectorVisual)
}


fn lerp(a: u8, b: u8, t: f32) -> u8 {
    ((1.0 - t) * a as f32 + t * b as f32).round() as u8
}

impl FixtureMode{
    fn get_rgb_at(&self, steps: f32) -> Color {
        match self {
            Self::Transparent => Color::new(0, 0, 0),
            Self::On { color } => *color,
            Self::ToggleOnSignal { a, b, signal_source } => {
                let alpha = signal_source.get_state_at(steps);
                let color_a = a.get_rgb_at(steps);
                let color_b = b.get_rgb_at(steps);
                
                Color {
                    r: lerp(color_a.r, color_b.r, alpha),
                    g: lerp(color_a.g, color_b.g, alpha),
                    b: lerp(color_a.b, color_b.b, alpha),
                }
            },
            Self::Projector(_) => {
                Color::new(0, 0, 0)
            }
        }
    }

    fn mode_equals(&self, other: &FixtureMode) -> bool {
        match (self, other) {
            (FixtureMode::Transparent, FixtureMode::Transparent) => true,
            (FixtureMode::On { color: c1 }, FixtureMode::On { color: c2 }) => c1 == c2,
            (FixtureMode::ToggleOnSignal { .. }, FixtureMode::ToggleOnSignal { .. }) => true,
            (FixtureMode::Projector(v1), FixtureMode::Projector(v2)) => v1 == v2,
            _ => false,
        }
    }
}

#[derive(Clone, Copy, Serialize, Deserialize)]
enum DMXFixtureType{
    Par,
    StrobeWash
}

impl DMXFixtureType {
    fn get_dmx(&self, channel: u16, color: Color) -> [u8; 512] {
        let mut dmx_data = [0; 512];
        let channel = channel as usize - 1; // DMX channels are 1-indexed, but our array is 0-indexed
        match self {
            DMXFixtureType::Par => {
                if channel + 2 < 512 {
                    dmx_data[channel] = color.r;
                    dmx_data[channel + 1] = color.g;
                    dmx_data[channel + 2] = color.b;
                }
            },
            DMXFixtureType::StrobeWash => {
                if channel + 3 < 512 {
                    dmx_data[channel] = color.r;
                    dmx_data[channel + 1] = color.g;
                    dmx_data[channel + 2] = color.b;
                    dmx_data[channel + 3] = 255; // Assuming full intensity for now
                }
            },
        }
        dmx_data
    }
}

#[derive(Clone, Serialize, Deserialize)]
enum Fixture {
    Projector{position: Option<Position3D>, mode:FixtureMode},
    DMXFixture{fixture_type:DMXFixtureType, position: Option<Position3D>, mode:FixtureMode, dmx_channel:u16},
}

impl Fixture {
    fn new_projector(position:Option<Position3D>)-> Fixture{
        Fixture::Projector { position, mode: FixtureMode::Transparent}
    }

    fn new_dmx_fixture(position:Option<Position3D>, fixture_type:DMXFixtureType, dmx_channel: u16)-> Fixture{
        Fixture::DMXFixture { fixture_type, position, mode: FixtureMode::Transparent, dmx_channel}
    }

    fn get_color_at(&self, steps: f32) -> Color{
        match self {
            Self::DMXFixture { fixture_type, position, mode, dmx_channel } => {
                mode.get_rgb_at(steps)
            },
            Self::Projector { position, mode } => mode.get_rgb_at(steps)
        }
    }

    fn get_mode(&self) -> &FixtureMode {
        match self {
            Fixture::Projector { mode, .. } => mode,
            Fixture::DMXFixture { mode, .. } => mode,
        }
    }

    fn get_dmx(&self, current_time: f32) -> [u8; 512] {
        match self {
            Fixture::DMXFixture { fixture_type, dmx_channel, .. } => {
                fixture_type.get_dmx(*dmx_channel, self.get_color_at(current_time*4.))
            },
            Fixture::Projector { .. } => [0; 512], // Projectors don't output DMX
        }
    }
}

enum AppMode {
    Normal,
    Save(String),
    Load(Vec<String>, usize),
    ComposeFile(Vec<String>, usize),
}

#[derive(Serialize, Deserialize)]
struct PatchState {
    fixtures: HashMap<Uuid, Fixture>,
}

struct Patch {
    fixtures:HashMap<Uuid,Fixture>,
    start: Instant,
    selection: HashSet<Uuid>,
    focus:Uuid,
    selection_mode: Option<FixtureMode>,
    app_mode: AppMode,
}

impl Patch{

    fn new(fixtures: Vec<Fixture>) -> Patch {
        let fixtures: HashMap<Uuid, Fixture> = fixtures.into_iter().map(|f| (Uuid::new_v4(), f)).collect();
        let focus = *fixtures.keys().next().unwrap();

        Patch {
            fixtures,
            start: Instant::now(),
            selection: HashSet::new(),
            focus,
            selection_mode:None,
            app_mode: AppMode::Normal,
        }
    }

    fn enter_compose_file_mode(&mut self) -> io::Result<()> {
        let presets_dir = PathBuf::from("presets");
        if !presets_dir.exists() {
            fs::create_dir_all(&presets_dir)?;
        }
    
        let files = fs::read_dir(presets_dir)?
            .filter_map(|entry| {
                entry.ok().and_then(|e| 
                    e.path().file_stem()
                        .and_then(|n| n.to_str().map(String::from))
                )
            })
            .collect::<Vec<String>>();
        self.app_mode = AppMode::ComposeFile(files, 0);
        Ok(())
    }

    fn compose_with_file(&mut self, file: &str) -> io::Result<()> {
        let presets_dir = PathBuf::from("presets");
        let file_path = presets_dir.join(format!("{}.json", file));
        let contents = fs::read_to_string(file_path)?;
        let state: PatchState = serde_json::from_str(&contents)?;

        if let Some(current_mode) = &self.selection_mode {
            let new_mode = FixtureMode::ToggleOnSignal {
                a: Box::new(current_mode.clone()),
                b: Box::new(FixtureMode::Transparent), // Default to transparent for 'b'
                signal_source: SignalSource {
                    shift:0,
                    steps: 4,
                    pulses: 2,
                    transition: Transition::Instant,
                },
            };

            for id in &self.selection {
                if let Some(fixture) = self.fixtures.get_mut(id) {
                    match fixture {
                        Fixture::DMXFixture { mode, .. } | Fixture::Projector { mode, .. } => {
                            *mode = new_mode.clone();
                        }
                    }
                }
            }
        }
        self.update_selection_mode();
        Ok(())
    }

    fn enter_save_mode(&mut self) {
        self.app_mode = AppMode::Save(String::new());
    }

    fn enter_load_mode(&mut self) -> io::Result<()> {
        let presets_dir = PathBuf::from("presets");
        if !presets_dir.exists() {
            fs::create_dir_all(&presets_dir)?;
        }
        
        let files = fs::read_dir(presets_dir)?
            .filter_map(|entry| {
                entry.ok().and_then(|e| 
                    e.path().file_stem()
                        .and_then(|n| n.to_str().map(String::from))
                )
            })
            .collect::<Vec<String>>();
        self.app_mode = AppMode::Load(files, 0);
        Ok(())
    }

    fn save_preset(&mut self, name: &str) -> io::Result<()> {
        let state = PatchState {
            fixtures: self.fixtures.clone(),
        };
        let serialized = serde_json::to_string(&state)?;
        let presets_dir = PathBuf::from("presets");
        fs::create_dir_all(&presets_dir)?;
        let file_path = presets_dir.join(format!("{}.json", name));
        fs::write(file_path, serialized)?;
        Ok(())
    }

    fn load_preset(&mut self, name: &str) -> io::Result<()> {
        let presets_dir = PathBuf::from("presets");
        let file_path = presets_dir.join(format!("{}.json", name));
        let contents = fs::read_to_string(file_path)?;
        let state: PatchState = serde_json::from_str(&contents)?;
        self.fixtures = state.fixtures;
        self.update_selection_mode();
        
        // Ensure focus is set to a valid fixture
        if !self.fixtures.contains_key(&self.focus) {
            self.focus = *self.fixtures.keys().next().unwrap_or(&self.focus);
        }
        
        Ok(())
    }

    fn update_selection_mode(&mut self) {
        if self.selection.is_empty() {
            self.selection_mode = None;
            return;
        }

        let first_id = self.selection.iter().next().unwrap();
        let first_mode = self.fixtures[first_id].get_mode().clone();
        
        if self.selection.iter().all(|id| self.fixtures[id].get_mode().mode_equals(&first_mode)) {
            self.selection_mode = Some(first_mode);
        } else {
            self.selection_mode = None;
        }
    }

    fn modify_selection(&mut self) {
        if let Some(new_mode) = self.selection_mode.clone() {
            for id in &self.selection {
                if let Some(fixture) = self.fixtures.get_mut(id) {
                    match fixture {
                        Fixture::DMXFixture { mode, .. } | Fixture::Projector { mode, .. } => {
                            match (&new_mode, &mut *mode) {
                                (FixtureMode::ToggleOnSignal { signal_source: new_signal_source, .. }, 
                                 FixtureMode::ToggleOnSignal { signal_source, .. }) => {
                                    // Only update the signal_source for existing ToggleOnSignal modes
                                    *signal_source = new_signal_source.clone();
                                },
                                _ => *mode = new_mode.clone(),
                            }
                        }
                    }
                }
            }
        }
    }

    fn compose(&mut self, file: &str) -> io::Result<()> {
        let presets_dir = PathBuf::from("presets");
        let file_path = presets_dir.join(format!("{}.json", file));
        let contents = fs::read_to_string(file_path)?;
        let state: PatchState = serde_json::from_str(&contents)?;
    
        if let Some(current_mode) = &self.selection_mode {
            let new_mode = FixtureMode::ToggleOnSignal {
                a: Box::new(current_mode.clone()),
                b: Box::new(FixtureMode::Transparent),
                signal_source: SignalSource {
                    shift: 0,
                    steps: 4,
                    pulses: 2,
                    transition: Transition::Instant,
                },
            };
    
            for id in &self.selection {
                if let Some(fixture) = self.fixtures.get_mut(id) {
                    match fixture {
                        Fixture::DMXFixture { mode, .. } | Fixture::Projector { mode, .. } => {
                            *mode = new_mode.clone();
                        }
                    }
                }
            }
        }
        self.update_selection_mode();
        Ok(())
    }



    fn is_selection_all_projectors(&self) -> bool {
        (!self.selection.is_empty()) && self.selection.iter().all(|id| 
            matches!(self.fixtures.get(id), Some(Fixture::Projector { .. }))
        )
    }

    fn toggle_selection(&mut self) {
        if self.selection.contains(&self.focus) {
            self.selection.remove(&self.focus);
        } else {
            self.selection.insert(self.focus);
        }
        self.update_selection_mode();
    }

    fn focus_next(&mut self) {
        let keys: Vec<Uuid> = self.fixtures.keys().cloned().collect();
        if keys.is_empty() {
            return;
        }
        let current_idx = keys.iter().position(|&id| id == self.focus).unwrap_or(0);
        let next_idx = (current_idx + 1) % keys.len();
        self.focus = keys[next_idx];
    }
    
    fn focus_prev(&mut self) {
        let keys: Vec<Uuid> = self.fixtures.keys().cloned().collect();
        if keys.is_empty() {
            return;
        }
        let current_idx = keys.iter().position(|&id| id == self.focus).unwrap_or(0);
        let prev_idx = if current_idx == 0 {
            keys.len() - 1
        } else {
            current_idx - 1
        };
        self.focus = keys[prev_idx];
    }

    fn select_all_dmx_fixtures(&mut self) {
        self.selection.clear();
        for (id, fixture) in &self.fixtures {
            if matches!(fixture, Fixture::DMXFixture { .. }) {
                self.selection.insert(*id);
            }
        }
        self.update_selection_mode();
    }

    fn select_all_projectors(&mut self) {
        self.selection.clear();
        for (id, fixture) in &self.fixtures {
            if matches!(fixture, Fixture::Projector { .. }) {
                self.selection.insert(*id);
            }
        }
        self.update_selection_mode();
    }

    fn select_all(&mut self) {
        self.selection = self.fixtures.keys().cloned().collect();
        self.update_selection_mode();
    }

    fn save(&self) -> Result<(), Box<dyn std::error::Error>> {
        let state = PatchState {
            fixtures: self.fixtures.clone(),
        };
        let serialized = serde_json::to_string(&state)?;
        fs::create_dir_all("presets")?;
        fs::write("presets/latest.json", serialized)?;
        Ok(())
    }

    fn load(&mut self, file: &str) -> Result<(), Box<dyn std::error::Error>> {
        let path = Path::new("presets").join(file);
        let contents = fs::read_to_string(path)?;
        let state: PatchState = serde_json::from_str(&contents)?;
        self.fixtures = state.fixtures;
        self.update_selection_mode();
        Ok(())
    }


}


/*
IMPORTANT FOR AI:

Controls:

BASICS
j : focus next
k : focus prev
space : toggle selection of focus
m : iterate mode between (off, on, {projector iff only projectors selected})
s : save
l : load

EDIT
o : toggle between color and transparent
c : compose
p: projector/iterate projector mode

ON SETTINGS
{r|g|b}+j : decrease {red|green|blue} value of on mode
{r|g|b}+k : increase {red|green|blue} value of on mode

COMPOSE SETTINGS
x+j : decrease signal-source-steps
x+k : increase signal-source-steps
f+j : decrease signal-source-pulses
f+k : increase signal-source-pulses
t : iterate transition between (instant, interpolate)


Interface:
FIXTURE OVERVIEW
Overview of all of the fixtures. Layout groups dmx-fixtures, projectors, and par-lights separately. 
Color indicate their current color (that can be gotten with the get_current_color method). Each fixture representation
idicates if it is selected or in focus, or both

ON SETTINGS:
displays rgb of on mode

COMPOSE SETTINGS:
Indicate urrent mode (euclidean rhythm or audio frequency)
Visualization of euclidean rhythm
frequency and threshold for audio frequency mode
Indicate transition type and duration if relevant
*/


fn main() -> Result<(), io::Error> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let fixtures = vec![
        Fixture::new_projector(None),
        Fixture::new_dmx_fixture(None, DMXFixtureType::StrobeWash, 6),
        Fixture::new_dmx_fixture(None, DMXFixtureType::StrobeWash, 1),
    ];

    let mut patch = Patch::new(fixtures);

    // Set up DMX;
    let mut dmx_port = &mut available_ports().unwrap()[1];
    dmx_port.open().unwrap();

    let tick_rate = Duration::from_millis(30);
    let mut last_tick = Instant::now();
    let mut last_dmx_send = Instant::now();
    let mut combine_key: Option<char> = None;

    loop {
        terminal.draw(|f| ui(f, &mut patch))?;

        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));

        if crossterm::event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
                match patch.app_mode {
                    AppMode::Normal => {
                        match key.code {
                            KeyCode::Char('q') => break,
                            KeyCode::Char('j') => {
                                if let Some(combine) = combine_key {
                                    match combine {
                                        'r' | 'g' | 'b' => {
                                            if let Some(FixtureMode::On { color }) = &mut patch.selection_mode {
                                                match combine {
                                                    'r' => color.r = color.r.saturating_sub(1),
                                                    'g' => color.g = color.g.saturating_sub(1),
                                                    'b' => color.b = color.b.saturating_sub(1),
                                                    _ => unreachable!(),
                                                }
                                                patch.modify_selection();
                                            }
                                        },
                                        'z' | 'x' => {
                                            if let Some(FixtureMode::ToggleOnSignal { signal_source, .. }) = &mut patch.selection_mode {
                                                match combine {
                                                    'z' => signal_source.steps = signal_source.steps.saturating_sub(1),
                                                    'x' => signal_source.pulses = signal_source.pulses.saturating_sub(1),
                                                    _ => unreachable!(),
                                                }
                                                patch.modify_selection();
                                            }
                                        },
                                        't' => {
                                            if let Some(FixtureMode::ToggleOnSignal { signal_source, .. }) = &mut patch.selection_mode {
                                                signal_source.transition = match signal_source.transition {
                                                    Transition::Instant => Transition::Interpolate { ease_in: true, ease_out: false},
                                                    Transition::Interpolate { ease_in: false, ease_out: false } => Transition::Interpolate { ease_in: true, ease_out: false },
                                                    Transition::Interpolate { ease_in: true, ease_out: false } => Transition::Interpolate { ease_in: false, ease_out: true },
                                                    Transition::Interpolate { ease_in: false, ease_out: true } => Transition::Interpolate { ease_in: true, ease_out: true },
                                                    Transition::Interpolate { ease_in: true, ease_out: true, .. } => Transition::Instant,
                                                };
                                                patch.modify_selection();
                                            }
                                        },
                                        _ => {}
                                    }
                                } else {
                                    patch.focus_next();
                                }
                            }
                            KeyCode::Char('k') => {
                                if let Some(combine) = combine_key {
                                    match combine {
                                        'r' | 'g' | 'b' => {
                                            if let Some(FixtureMode::On { color }) = &mut patch.selection_mode {
                                                match combine {
                                                    'r' => color.r = color.r.add(1).min(255),
                                                    'g' => color.g = color.g.add(1).min(255),
                                                    'b' => color.b = color.b.add(1).min(255),
                                                    _ => unreachable!(),
                                                }
                                                patch.modify_selection();
                                            }
                                        },
                                        'z' | 'x' => {
                                            if let Some(FixtureMode::ToggleOnSignal { signal_source, .. }) = &mut patch.selection_mode {
                                                match combine {
                                                    'z' => signal_source.steps = signal_source.steps.saturating_add(1).min(32),
                                                    'x' => signal_source.pulses = signal_source.pulses.saturating_add(1).min(signal_source.steps),
                                                    _ => unreachable!(),
                                                }
                                                patch.modify_selection();
                                            }
                                        },
                                        _ => {}
                                    }
                                } else {
                                    patch.focus_prev();
                                }
                            }
                            KeyCode::Char('t') => {
                                if let Some(FixtureMode::ToggleOnSignal { signal_source, .. }) = &mut patch.selection_mode {
                                    signal_source.transition = match signal_source.transition {
                                        Transition::Instant => Transition::Interpolate { ease_in: false, ease_out: false},
                                        Transition::Interpolate { ease_in: false, ease_out: false } => Transition::Interpolate { ease_in: true, ease_out: false },
                                        Transition::Interpolate { ease_in: true, ease_out: false } => Transition::Interpolate { ease_in: false, ease_out: true },
                                        Transition::Interpolate { ease_in: false, ease_out: true } => Transition::Interpolate { ease_in: true, ease_out: true },
                                        Transition::Interpolate { ease_in: true, ease_out: true } => Transition::Instant,
                                    };
                                    patch.modify_selection();
                                }
                            },

                            KeyCode::Char(' ') => patch.toggle_selection(),
                            KeyCode::Char('o') => {
                                if !patch.selection.is_empty() {
                                    patch.selection_mode = match &patch.selection_mode {
                                        Some(FixtureMode::On { color: _ }) => Some(FixtureMode::Transparent),
                                        Some(FixtureMode::Transparent) => Some(FixtureMode::On { color: Color::new(100, 100, 100) }),
                                        _ => Some(FixtureMode::On { color: Color::new(100, 100, 100) })
                                    };
                                    patch.modify_selection();
                                }
                            },
                            KeyCode::Char('p') => {
                                if patch.is_selection_all_projectors() {
                                    patch.selection_mode = match &patch.selection_mode {
                                        Some(FixtureMode::Projector(ProjectorVisual::BlackAndWhite)) => {
                                            Some(FixtureMode::Projector(ProjectorVisual::Camera))
                                        },
                                        Some(FixtureMode::Projector(ProjectorVisual::Camera)) => {
                                            Some(FixtureMode::Projector(ProjectorVisual::BlackAndWhite))
                                        },
                                        _ => Some(FixtureMode::Projector(ProjectorVisual::BlackAndWhite)),
                                    };
                                    patch.modify_selection();
                                }
                            },
                            KeyCode::Char('c') => {
                                patch.enter_compose_file_mode()?
                            },
                            KeyCode::Char('r') | KeyCode::Char('g') | KeyCode::Char('b') | KeyCode::Char('x') | KeyCode::Char('c') | KeyCode::Char('f')| KeyCode::Char('z') => {
                                combine_key = Some(match key.code {
                                    KeyCode::Char(c) => c,
                                    _ => unreachable!(),
                                });
                            },
                            KeyCode::Char('s') => patch.enter_save_mode(),
                            KeyCode::Char('l') => patch.enter_load_mode()?,
                            KeyCode::Char('a') => patch.select_all(),
                            KeyCode::Esc => {
                                combine_key = None;
                            }
                            _ => {}
                        }
                    },
                    AppMode::Save(ref mut name) => {
                        match key.code {
                            KeyCode::Char(c) => name.push(c),
                            KeyCode::Backspace => { name.pop(); },
                            KeyCode::Enter => {
                                let save_name = name.clone();
                                patch.save_preset(&save_name)?;
                                patch.app_mode = AppMode::Normal;
                            },
                            KeyCode::Esc => patch.app_mode = AppMode::Normal,
                            _ => {}
                        }
                    },
                    AppMode::Load(ref files, ref mut selected) => {
                        match key.code {
                            KeyCode::Up => *selected = selected.saturating_sub(1),
                            KeyCode::Down => *selected = (*selected + 1).min(files.len().saturating_sub(1)),
                            KeyCode::Enter => {
                                if let Some(file) = files.get(*selected) {
                                    let load_name = file.clone();
                                    patch.load_preset(&load_name)?;
                                }
                                patch.app_mode = AppMode::Normal;
                            },
                            KeyCode::Esc => patch.app_mode = AppMode::Normal,
                            _ => {}
                        }
                    },
                    AppMode::ComposeFile(ref files, ref mut selected) => {
                        match key.code {
                            KeyCode::Up => *selected = selected.saturating_sub(1),
                            KeyCode::Down => *selected = (*selected + 1).min(files.len().saturating_sub(1)),
                            KeyCode::Enter => {
                                if let Some(file) = files.get(*selected).cloned() {
                                    patch.compose(&file)?;
                                    patch.app_mode = AppMode::Normal;
                                }
                            },
                            KeyCode::Esc => patch.app_mode = AppMode::Normal,
                            _ => {}
                        }
                    },
        
                }
            }
        }

        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
        }

        // Send DMX data at 30 fps
        if last_dmx_send.elapsed() >= Duration::from_millis(33) {
            let current_time = patch.start.elapsed().as_secs_f32();
            let mut dmx_buffer = [0u8; 512];

            for fixture in patch.fixtures.values() {
                if let Fixture::DMXFixture { .. } = fixture {
                    let fixture_dmx = fixture.get_dmx(current_time);
                    for (i, &value) in fixture_dmx.iter().enumerate() {
                        dmx_buffer[i] = value.max(dmx_buffer[i]);
                    }
                }
            }

            dmx_port.write(&dmx_buffer).unwrap();//sometimes this fails TODO

            last_dmx_send = Instant::now();
        }
    }

    patch.modify_selection();

    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    Ok(())
}

fn ui<B: Backend>(f: &mut tui::Frame<B>, patch: &Patch) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(1)
        .constraints(
            [
                Constraint::Percentage(50),
                Constraint::Percentage(25),
                Constraint::Percentage(25),
            ]
            .as_ref(),
        )
        .split(f.size());

    // Fixture Overview
    let fixture_overview: Vec<ListItem> = patch.fixtures.iter().map(|(id, fixture)| {
        let mut style = Style::default();
        if patch.selection.contains(id) {
            style = style.add_modifier(Modifier::REVERSED);
        }
        if *id == patch.focus {
            style = style.add_modifier(Modifier::BOLD);
        }
        let color = fixture.get_color_at((patch.start.elapsed().as_millis() as f32 /1000.));
        let text = match fixture {
            Fixture::Projector { .. } => "Projector",
            Fixture::DMXFixture { fixture_type, .. } => match fixture_type {
                DMXFixtureType::Par => "Par Light",
                DMXFixtureType::StrobeWash => "Strobe Wash",
            },
        };
        ListItem::new(Span::styled(text, style.fg(TUIColor::Rgb(color.r, color.g, color.b))))
    }).collect();

    let fixture_list = List::new(fixture_overview)
        .block(Block::default().borders(Borders::ALL).title("Fixtures"));
    f.render_widget(fixture_list, chunks[0]);


    match &patch.app_mode {
        AppMode::Normal => {
            if !patch.selection.is_empty() {
                match &patch.selection_mode {
                    Some(FixtureMode::On { color }) => {
                        // On settings
                        let on_settings_text = format!(
                            "Color:\n  R: {:.2}\n  G: {:.2}\n  B: {:.2}",
                            color.r, color.g, color.b
                        );
                        let on_settings = Paragraph::new(on_settings_text)
                            .block(Block::default().borders(Borders::ALL).title("On settings"));
                        f.render_widget(on_settings, chunks[1]);
                    },
        
                    Some(FixtureMode::Projector(pv)) => {
                        let mode = match pv {
                            ProjectorVisual::Camera => "Camera",
                            ProjectorVisual::BlackAndWhite => "B&W"
                        };
        
                        // let projector_settings_text = 
                        let projector_settings = Paragraph::new(mode)
                            .block(Block::default().borders(Borders::ALL).title("Projector"));
                        f.render_widget(projector_settings, chunks[1]);
                    },
        
                    Some(FixtureMode::ToggleOnSignal { a, b, signal_source }) => {
                        // Compose Settings text
                        let compose_settings_text = format!(
                            "Shift: {}\n\
                            Steps: {}\n\
                            Pulses: {}\n\
                            Visualization: {}\n\
                            Transition: {}",
                            signal_source.shift, signal_source.steps, signal_source.pulses,
                            "□".repeat(signal_source.steps as usize).replace(&"□".repeat(signal_source.pulses as usize), "■"),
                            signal_source.transition
                        );
                    
                        let compose_settings = Paragraph::new(compose_settings_text)
                            .block(Block::default().borders(Borders::ALL).title("Compose Settings"));
                        f.render_widget(compose_settings, chunks[1]);
                    
                        // Signal Graph
                        let resolution = 100; // Number of points to sample for the graph
                        let signal_values = signal_source.represent(resolution);
                        
                        let data: Vec<(f64, f64)> = signal_values.iter().enumerate()
                            .map(|(i, &value)| (i as f64 / resolution as f64, value as f64))
                            .collect();

                        // Create beat markers
                        let beat_data: Vec<(f64, f64)> = (0..=signal_source.steps)
                            .map(|i| {
                                let x = i as f64 / signal_source.steps as f64;
                                (x, 0.0) // Place markers at the bottom of the chart
                            })
                            .collect();

                        let datasets = vec![
                            Dataset::default()
                                .name("Signal")
                                .marker(symbols::Marker::Braille)
                                .graph_type(tui::widgets::GraphType::Line)
                                .style(Style::default().fg(TUIColor::Cyan))
                                .data(&data),
                            Dataset::default()
                                .name("Beats")
                                .marker(symbols::Marker::Block)
                                .graph_type(tui::widgets::GraphType::Scatter)
                                .style(Style::default().fg(TUIColor::Red))
                                .data(&beat_data)
                        ];

                        let chart = Chart::new(datasets)
                            .block(Block::default().title("Signal Graph").borders(Borders::ALL))
                            .x_axis(Axis::default()
                                .title("Time")
                                .style(Style::default().fg(TUIColor::Gray))
                                .bounds([0.0, 1.0])
                                .labels(vec!["0".into(), "1".into()]))
                            .y_axis(Axis::default()
                                .title("Value")
                                .style(Style::default().fg(TUIColor::Gray))
                                .bounds([0.0, 1.0])
                                .labels(vec!["0".into(), "1".into()]));

                        f.render_widget(chart, chunks[2]);
                    },
        
                    Some(FixtureMode::Transparent) => {
                        let transparent = Paragraph::new("")
                            .block(Block::default().borders(Borders::ALL).title("Transparent"));
                        f.render_widget(transparent, chunks[1]);
                    },
        
                    _ => ()
                }
            }
        },
        AppMode::Save(name) => {
            let save_widget = Paragraph::new(name.as_str())
                .block(Block::default().borders(Borders::ALL).title("Save Preset"));
            f.render_widget(save_widget, chunks[1]);

            let instructions = Paragraph::new("Enter name and press Enter to save. Esc to cancel.")
                .block(Block::default().borders(Borders::ALL).title("Instructions"));
            f.render_widget(instructions, chunks[2]);
        },
        AppMode::Load(files, selected) => {
            let items: Vec<ListItem> = files.iter().enumerate().map(|(i, name)| {
                if i == *selected {
                    ListItem::new(name.as_str()).style(Style::default().add_modifier(Modifier::REVERSED))
                } else {
                    ListItem::new(name.as_str())
                }
            }).collect();

            let load_widget = List::new(items)
                .block(Block::default().borders(Borders::ALL).title("Load Preset"));
            f.render_widget(load_widget, chunks[1]);

            let instructions = Paragraph::new("Use Up/Down to select, Enter to load. Esc to cancel.")
                .block(Block::default().borders(Borders::ALL).title("Instructions"));
            f.render_widget(instructions, chunks[2]);
        },
        AppMode::ComposeFile(files, selected) => {
            let items: Vec<ListItem> = files.iter().enumerate().map(|(i, name)| {
                if i == *selected {
                    ListItem::new(name.as_str()).style(Style::default().add_modifier(Modifier::REVERSED))
                } else {
                    ListItem::new(name.as_str())
                }
            }).collect();
        
            let compose_file_widget = List::new(items)
                .block(Block::default().borders(Borders::ALL).title("Select File for Composition"));
            f.render_widget(compose_file_widget, chunks[1]);
        
            let instructions = Paragraph::new("Use Up/Down to select, Enter to compose. Esc to cancel.")
                .block(Block::default().borders(Borders::ALL).title("Instructions"));
            f.render_widget(instructions, chunks[2]);
        }
    }
    
}


/*
TODO:
[x] Change the actual state (colors, compositions, etc)
[x] Save
[x] Load
[x] Select all
[x] Compose Choose File
[x] SignalSource
[x] Color getters and mixing
[x] get_dmx_buffer method
[x] get_dmx methods for all the dmx fixtures
[x] sends dmx with rust_dmx
[] speed multiplier
[] Bug wth mixing and transparency
*/