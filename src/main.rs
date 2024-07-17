use core::fmt;
use std::{collections::{HashMap, HashSet}, fmt::{Display, Formatter}, fs::File, ops::Add};
use uuid::Uuid;
use std::time::{Duration, Instant};
use rust_dmx::{available_ports, DmxPort};

use tui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout},
    style::{Color as TUIColor, Modifier, Style},
    text::{Span, Spans},
    widgets::{Block, Borders, List, ListItem, Paragraph},
    Terminal,
};
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};

use std::io;

#[derive(Clone, Copy, PartialEq)]
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

#[derive(Clone, Copy)]
struct Position3D{
    x:f32,
    y:f32,
    z:f32
}

#[derive(Clone, Copy, Debug)]
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

#[derive(Clone, Copy, Debug)]
enum AudioFrequencyBand{
    Low,
    Mid,
    High
}

#[derive(Clone, Copy)]
struct SignalSource {
    speed: u8, 
    steps: u8, 
    pulses: u8, 
    transition: Transition
}

impl SignalSource{
    fn get_state_at(self, time:Duration) -> f32{
        todo!();
    }
}

#[derive(Clone, Copy)]
struct ProjetorSourceWindow{
    position: (f32,f32),
    scale: f32,
    rotation: f32
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum ProjectorVisual {
    Camera,
    BlackAndWhite
}

impl ProjectorVisual {
}

#[derive(Clone)]
enum FixtureMode{
    Transparent,
    On{color:Color},
    ToggleOnSignal{a:Box<FixtureMode>, b:Box<FixtureMode>, signal_source:SignalSource},
    Projector(ProjectorVisual)
}


impl FixtureMode{
    fn get_rgb_at(self, time: Duration) -> Color{
        match self {
            Self::Transparent => Color::new(0,0,0),
            Self::On { color } => color,
            Self::ToggleOnSignal { a, b, signal_source } => {
                todo!();
            },
            Self::Projector(_) => {
                todo!();
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

#[derive(Clone, Copy)]
enum DMXFixtureType{
    Par,
    StrobeWash
}

#[derive(Clone)]
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

    fn get_color_at(&self, time: Duration) -> Color{
        //TODO
        Color::new(200, 200, 50)
    }

    fn get_mode(&self) -> &FixtureMode {
        match self {
            Fixture::Projector { mode, .. } => mode,
            Fixture::DMXFixture { mode, .. } => mode,
        }
    }
}

struct Patch {
    fixtures:HashMap<Uuid,Fixture>,
    start: Instant,
    selection: HashSet<Uuid>,
    focus:Uuid,
    selection_mode: Option<FixtureMode>
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
        }
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

    fn compose(&mut self) {
        if let Some(current_mode) = &self.selection_mode {
            let new_mode = FixtureMode::ToggleOnSignal {
                a: Box::new(current_mode.clone()),
                b: Box::new(FixtureMode::Transparent), // Default to transparent for 'b'
                signal_source: SignalSource {
                    speed: 1,
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
        let current_idx = keys.iter().position(|&id| id == self.focus).unwrap();
        let next_idx = (current_idx + 1) % keys.len();
        self.focus = keys[next_idx];
    }

    fn focus_prev(&mut self) {
        let keys: Vec<Uuid> = self.fixtures.keys().cloned().collect();
        let current_idx = keys.iter().position(|&id| id == self.focus).unwrap();
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

    fn save(&self) {
        // serialize and save fixtures to /presets
    }

    fn load(&mut self) {
        // load fixtures from file from /presets
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
        Fixture::new_dmx_fixture(None, DMXFixtureType::Par, 1),
        Fixture::new_dmx_fixture(None, DMXFixtureType::StrobeWash, 2),
        Fixture::new_dmx_fixture(None, DMXFixtureType::StrobeWash, 6),
    ];

    let mut patch = Patch::new(fixtures);

    let tick_rate = Duration::from_millis(33);
    let mut last_tick = Instant::now();
    let mut combine_key: Option<char> = None;

    loop {
        terminal.draw(|f| ui(f, & mut patch))?;

        let timeout = tick_rate
            .checked_sub(last_tick.elapsed())
            .unwrap_or_else(|| Duration::from_secs(0));

        if crossterm::event::poll(timeout)? {
            if let Event::Key(key) = event::read()? {
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
                                'z' | 'x' | 'f' => {
                                    if let Some(FixtureMode::ToggleOnSignal { signal_source, .. }) = &mut patch.selection_mode {
                                        match combine {
                                            'z' => signal_source.steps = signal_source.steps.saturating_sub(1),
                                            'x' => signal_source.pulses = signal_source.pulses.saturating_sub(1),
                                            'f' => signal_source.speed = signal_source.speed.saturating_sub(1),
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
                                'z' | 'x' | 'f' => {
                                    if let Some(FixtureMode::ToggleOnSignal { signal_source, .. }) = &mut patch.selection_mode {
                                        match combine {
                                            'z' => signal_source.steps = signal_source.steps.saturating_add(1).min(32),
                                            'x' => signal_source.pulses = signal_source.pulses.saturating_add(1).min(signal_source.steps),
                                            'f' => signal_source.speed = signal_source.speed.saturating_add(1).min(255),
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
                    KeyCode::Char('s') => patch.save(),
                    KeyCode::Char('l') => patch.load(),
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
                        patch.compose();
                        patch.modify_selection();
                    },
                    KeyCode::Char('r') | KeyCode::Char('g') | KeyCode::Char('b') | KeyCode::Char('x') | KeyCode::Char('c') | KeyCode::Char('f')| KeyCode::Char('z') => {
                        combine_key = Some(match key.code {
                            KeyCode::Char(c) => c,
                            _ => unreachable!(),
                        });
                    }
                    KeyCode::Esc => {
                        combine_key = None;
                    }
                    _ => {}
                }
            }
        }

        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
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
        let color = fixture.get_color_at(patch.start.elapsed());
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
                // Compose Settings
                let compose_settings_text = format!(
                    "Speed: {}\n\
                    Steps: {}\n\
                    Pulses: {}\n\
                    Visualization: {}\n\
                    Transition: {}",
                    signal_source.speed, signal_source.steps, signal_source.pulses,
                    "□".repeat(signal_source.steps as usize).replace(&"□".repeat(signal_source.pulses as usize), "■"),
                    signal_source.transition
                );
            
                let compose_settings = Paragraph::new(compose_settings_text)
                    .block(Block::default().borders(Borders::ALL).title("Compose Settings"));
                f.render_widget(compose_settings, chunks[1]);
            },

            Some(FixtureMode::Transparent) => {
                let transparent = Paragraph::new("")
                    .block(Block::default().borders(Borders::ALL).title("Transparent"));
                f.render_widget(transparent, chunks[1]);
            },

            _ => ()
        }
    }
    
}


/*
TODO:
[x] Change the actual state (colors, compositions, etc)
[] Save
[] Load
[] Compose Choose File
[] SignalSource
[] get_dmx_buffer method
[] thread that sends dmx
*/