use std::collections::{HashMap, HashSet};
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

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
enum Transition{
    Instant,
    Linear{time:Duration},
    Ease{ease_in:bool, ease_out:bool, time:Duration},
}

#[derive(Clone, Copy)]
enum SignalSource{
    EuclideanRhythm{frequency: f32, steps: u8, pulses: u8},
    AudioFrequency{frequency: f32, threshold: f32}
}

impl SignalSource{
    fn get_state_at(self, time:Duration) -> bool{
        match self {
            Self::EuclideanRhythm { frequency, steps, pulses } => {
                todo!();
            },
            Self::AudioFrequency { frequency, threshold } => {
                todo!();
            }
        }
    }
}

#[derive(Clone, Copy)]
struct ProjetorSourceWindow{
    position: (f32,f32),
    scale: f32,
    rotation: f32
}

#[derive(Clone, Copy)]
enum ProjectorVisual {
    ThreeWindows{
        window_a:ProjetorSourceWindow,
        window_b:ProjetorSourceWindow,
        window_c:ProjetorSourceWindow,
        camera_source: i32
    },
    VideoLoop
}

impl ProjectorVisual {
}

#[derive(Clone)]
enum FixtureMode{
    Off,
    On{color:Color},
    ToggleOnSignal{a:Box<FixtureMode>, b:Box<FixtureMode>, signal_source:SignalSource, transition:Transition},
    Projector //TODO:add window handle
}


impl FixtureMode{
    fn get_rgb_at(self, time: Duration) -> Color{
        match self {
            Self::Off => Color::new(0,0,0),
            Self::On { color } => color,
            Self::ToggleOnSignal { a, b, signal_source , transition} => {
                todo!();
            },
            Self::Projector => {
                todo!();
            }
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
        Fixture::Projector { position, mode: FixtureMode::Off}
    }

    fn new_dmx_fixture(position:Option<Position3D>, fixture_type:DMXFixtureType, dmx_channel: u16)-> Fixture{
        Fixture::DMXFixture { fixture_type, position, mode: FixtureMode::Off, dmx_channel}
    }

    fn get_color_at(&self, time: Duration) -> Color{
        //TODO
        Color::new(200, 10, 50)
    }
}


struct PatchOnMode {
    r: f32,
    g: f32,
    b: f32,
}

impl PatchOnMode {
    fn new() -> PatchOnMode {
        PatchOnMode { r: 0.0, g: 0.0, b: 0.0 }
    }
}

struct PatchEuclideanRhythm {
    steps: u8,
    pulses: u8,
}

impl PatchEuclideanRhythm {
    fn new() -> PatchEuclideanRhythm {
        PatchEuclideanRhythm { steps: 0, pulses: 0 }
    }
}

#[derive(Debug)]
enum PatchAudioFrequencyBand {
    Highs,
    Mids,
    Lows,
}

impl PatchAudioFrequencyBand {
    fn new() -> PatchAudioFrequencyBand {
        PatchAudioFrequencyBand::Mids
    }
}

struct PatchAudioFrequency {
    band: PatchAudioFrequencyBand,
    thresh: u8,
}

impl PatchAudioFrequency {
    fn new() -> PatchAudioFrequency {
        PatchAudioFrequency {
            band: PatchAudioFrequencyBand::new(),
            thresh: 0,
        }
    }
}

struct Patch {
    fixtures:HashMap<Uuid,Fixture>,
    start: Instant,
    selection: HashSet<Uuid>,
    focus:Uuid,
    on_mode: PatchOnMode,
    eucledean_rhythm: PatchEuclideanRhythm,
    audio_frequency: PatchAudioFrequency
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
            on_mode: PatchOnMode::new(),
            eucledean_rhythm:PatchEuclideanRhythm::new(),
            audio_frequency:PatchAudioFrequency::new()
        }
    }

    fn toggle_selection(&mut self) {
        if self.selection.contains(&self.focus) {
            self.selection.remove(&self.focus);
        } else {
            self.selection.insert(self.focus);
        }
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
    }

    fn select_all_projectors(&mut self) {
        self.selection.clear();
        for (id, fixture) in &self.fixtures {
            if matches!(fixture, Fixture::Projector { .. }) {
                self.selection.insert(*id);
            }
        }
    }

    fn increase_audio_frequency_frequency(&mut self) {
        self.audio_frequency.thresh = (self.audio_frequency.thresh + 1).min(100);
    }

    fn decrease_audio_frequency_frequency(&mut self) {
        self.audio_frequency.thresh = self.audio_frequency.thresh.saturating_sub(1);
    }

    fn iterate_audio_frequency_band(&mut self) {
        self.audio_frequency.band = match self.audio_frequency.band {
            PatchAudioFrequencyBand::Highs => PatchAudioFrequencyBand::Mids,
            PatchAudioFrequencyBand::Mids => PatchAudioFrequencyBand::Lows,
            PatchAudioFrequencyBand::Lows => PatchAudioFrequencyBand::Highs,
        };
    }

    fn increase_euclidean_rhythm_pulses(&mut self) {
        self.eucledean_rhythm.pulses = (self.eucledean_rhythm.pulses + 1).min(32);
    }

    fn decrease_euclidean_rhythm_pulses(&mut self) {
        self.eucledean_rhythm.pulses = self.eucledean_rhythm.pulses.saturating_sub(1);
    }

    fn increase_euclidean_rhythm_steps(&mut self) {
        self.eucledean_rhythm.steps = (self.eucledean_rhythm.steps + 1).min(32);
    }

    fn decrease_euclidean_rhythm_steps(&mut self) {
        self.eucledean_rhythm.steps = self.eucledean_rhythm.steps.saturating_sub(1);
    }

    fn save(&self) {
        // serialize and save fixtures to /presets
    }

    fn load(&mut self) {
        // load fixtures from file from /presets
    }

    fn compose(&self) {
        // load a fixtures from /presets, compose all modes
        todo!();
    }

}


/*

BASICS
j : focus next
k : focus prev
space : toggle selection of focus
m : iterate mode between (off, on, {projector iff only projectors selected})
s : save
l : load
c : compose

ON SETTINGS
{r|g|b}+j : decrease {red|green|blue} value of on mode
{r|g|b}+k : increase {red|green|blue} value of on mode

COMPOSE SETTINGS
z : toggle between (euclidean rhythm, audio frequency)
x+j : decrease euclidean-rhythm-steps and audio-frequency-band
x+k : increase euclidean-rhythm-steps and audio-frequency-band
c+j : decrease euclidean-rhythm-pulses and audio-frequency-threshold
c+k : increase euclidean-rhythm-pulses and audio-frequency-threshold
t : iterate transition between (instant, linear)
t+j : decrease transition suration
t+j : increase transition suration
*/

/*
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
    ];

    let mut patch = Patch::new(fixtures);

    let tick_rate = Duration::from_millis(33);
    let mut last_tick = Instant::now();
    let mut combine_key: Option<char> = None;

    loop {
        terminal.draw(|f| ui(f, &patch))?;

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
                                'r' => patch.on_mode.r = (patch.on_mode.r - 1.0).max(0.0),
                                'g' => patch.on_mode.g = (patch.on_mode.g - 1.0).max(0.0),
                                'b' => patch.on_mode.b = (patch.on_mode.b - 1.0).max(0.0),
                                'x' => patch.decrease_euclidean_rhythm_steps(),
                                'c' => patch.decrease_euclidean_rhythm_pulses(),
                                'f' => patch.decrease_audio_frequency_frequency(),
                                _ => {}
                            }
                        } else {
                            patch.focus_next();
                        }
                    }
                    KeyCode::Char('k') => {
                        if let Some(combine) = combine_key {
                            match combine {
                                'r' => patch.on_mode.r = (patch.on_mode.r + 1.0).min(255.0),
                                'g' => patch.on_mode.g = (patch.on_mode.g + 1.0).min(255.0),
                                'b' => patch.on_mode.b = (patch.on_mode.b + 1.0).min(255.0),
                                'x' => patch.increase_euclidean_rhythm_steps(),
                                'c' => patch.increase_euclidean_rhythm_pulses(),
                                'f' => patch.increase_audio_frequency_frequency(),
                                _ => {}
                            }
                        } else {
                            patch.focus_prev();
                        }
                    }
                    KeyCode::Char(' ') => patch.toggle_selection(),
                    KeyCode::Char('m') => {
                        // iterate mode
                    }
                    KeyCode::Char('s') => patch.save(),
                    KeyCode::Char('l') => patch.load(),
                    KeyCode::Char('c') => patch.compose(),
                    KeyCode::Char('r') | KeyCode::Char('g') | KeyCode::Char('b') | KeyCode::Char('x') | KeyCode::Char('c') | KeyCode::Char('f') => {
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

    // On Settings
    let on_settings_text = format!(
        "On Settings:\n  R: {:.2}\n  G: {:.2}\n  B: {:.2}",
        patch.on_mode.r, patch.on_mode.g, patch.on_mode.b
    );
    let on_settings = Paragraph::new(on_settings_text)
        .block(Block::default().borders(Borders::ALL).title("On Settings"));
    f.render_widget(on_settings, chunks[1]);

    // Compose Settings
    let compose_settings_text = format!(
        "Compose Settings:\n\
        Euclidean Rhythm:\n  Steps: {}\n  Pulses: {}\n\
        Audio Frequency:\n  Band: {:?}\n  Threshold: {}",
        patch.eucledean_rhythm.steps,
        patch.eucledean_rhythm.pulses,
        patch.audio_frequency.band,
        patch.audio_frequency.thresh
    );
    let compose_settings = Paragraph::new(compose_settings_text)
        .block(Block::default().borders(Borders::ALL).title("Compose Settings"));
    f.render_widget(compose_settings, chunks[2]);
}
