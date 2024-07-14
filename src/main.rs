use std::io::{self, Stdin, Stdout, Write};
use termion::input::TermRead;
use termion::raw::{IntoRawMode, RawTerminal};
use termion::event::Key;
use std::time::{Duration, Instant};
use std::thread;
use rust_dmx::{available_ports, DmxPort};


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

struct Position3D{
    x:f32,
    y:f32,
    z:f32
}

enum Transition{
    Instant,
    Linear{time:Duration},
    Ease{ease_in:bool, ease_out:bool, time:Duration},
}

enum SignalSource{
    RandomFlicker{mean_frequency: f32},
    Strobe{frequency: f32},
    EuclideanRhythm{frequency: f32, steps: u8, pulses: u8},
    Composite(Vec<SignalSource>)
}

impl SignalSource{
    fn get_state_at(self, time:Duration) -> bool{
        match self {
            Self::RandomFlicker { mean_frequency } => {
                todo!();
            },
            Self::Strobe { frequency } => {
                todo!();
            }, 
            Self::EuclideanRhythm { frequency, steps, pulses } => {
                todo!();
            },
            Self::Composite(composite) => {
                todo!();
            }
        }
    }
}

struct ProjetorSourceWindow{
    position: (f32,f32),
    scale: f32,
    rotation: f32
}

enum ProjectorVisual {
    ThreeWindows{
        window_a:ProjetorSourceWindow,
        window_b:ProjetorSourceWindow,
        window_c:ProjetorSourceWindow,
        camera_source: i32
    },
    VideoLoop{path:String}
}

impl ProjectorVisual {
    fn get_frame(&self, width: i32, height: i32) -> opencv::Result<Mat> {
        match self {
            ProjectorVisual::ThreeWindows {
                window_a,
                window_b,
                window_c,
                camera_source,
            } => {
                let mut cam = videoio::VideoCapture::new(*camera_source, videoio::CAP_ANY)?;
                if !videoio::VideoCapture::is_opened(&cam)? {
                    panic!("Unable to open camera");
                }

                let mut frame = Mat::default();
                cam.read(&mut frame)?;

                let mut display_frame = Mat::zeros(height, width, frame.typ()?)?.to_mat()?;

                Self::process_frame(&mut display_frame, &frame, window_a, width, height)?;
                Self::process_frame(&mut display_frame, &frame, window_b, width, height)?;
                Self::process_frame(&mut display_frame, &frame, window_c, width, height)?;

                Ok(display_frame)
            }
            ProjectorVisual::VideoLoop { path: _ } => {
                todo!()
            }
        }
    }

    fn process_frame(
        display_frame: &mut Mat,
        frame: &Mat,
        window: &ProjectorSourceWindow,
        width: i32,
        height: i32,
    ) -> opencv::Result<()> {
        let (frame_width, frame_height) = (frame.cols(), frame.rows());
        let new_size = Size::new(
            (frame_width as f32 * window.scale) as i32,
            (frame_height as f32 * window.scale) as i32,
        );
        let mut resized_frame = Mat::default();
        imgproc::resize(&frame, &mut resized_frame, new_size, 0.0, 0.0, imgproc::INTER_LINEAR)?;

        let center = Point2f::new(
            (resized_frame.cols() / 2) as f32,
            (resized_frame.rows() / 2) as f32,
        );
        let rot_mat = imgproc::get_rotation_matrix_2d(center, window.rotation * 180.0 / PI, 1.0)?;

        let mut rotated_frame = Mat::default();
        imgproc::warp_affine(
            &resized_frame,
            &mut rotated_frame,
            &rot_mat,
            new_size,
            imgproc::INTER_LINEAR,
            core::BORDER_CONSTANT,
            Scalar::all(0.0),
        )?;

        let pos_x = ((width as f32 / 2.0) + window.position.0 - (new_size.width as f32 / 2.0)) as i32;
        let pos_y = ((height as f32 / 2.0) + window.position.1 - (new_size.height as f32 / 2.0)) as i32;

        let roi = imgproc::get_rect_sub_pix(&mut display_frame, new_size, Point2f::new(pos_x as f32, pos_y as f32))?;
        rotated_frame.copy_to(&roi)?;

        Ok(())
    }
}

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

///

enum DMXFixtureType{
    Par,
    StrobeWash
}

enum Fixture {
    Projector{position: Option<Position3D>, mode:FixtureMode}, //todo add frame_buffer: Option<some_frame_buffer_type>
    DMXFixture{fixture_type:DMXFixtureType, position: Option<Position3D>, mode:FixtureMode, dmx_channel:u16},
}

// impl Fixture{
//     fn get_rgb(self, time: Duration) -> Color{
//         match self{
//             Self::Projector { position, mode } => {
//                 todo!()
//             },
//             Self::DMXFixture { fixture_type, position, mode, dmx_channel } => {
//                 todo!()
//             }
//         }
//     }
// }

struct Patch {
    fixtures:Vec<Fixture>,
    start: Instant,
    selection:Vec<&mut Fixture>
}

impl Patch {

    fn new()-> Patch{
        Patch{
            fixtures: vec![],
            start: Instant::now(),
            selection: vec![],
        }
    }

    //config
    fn add_fixture(self, fixture: Fixture){
        self.fixtures.append(fixture);
    }

    //runtime and updating

    fn tick(duration:Duration){

    }

    //basic operations
    //Off,
    //On{color:Color},
    //ToggleOnSignal{a:Box<FixtureMode>, b:Box<FixtureMode>, signal_source:SignalSource},
    //Projector //TODO:add window handle

    //Select group -> select mode (on, off, (projector)) -> settings for that mode
    //Compose -> select signal source -> select transition
    fn select_group(){

    }

    fn compose(){

    }

    //save patches
    //load patches
    //compose patches

    fn save(){

    }

    fn load(){

    }

    fn compose(){

    }

}


////

struct Model{
    patch: Patch
}

impl Model{
    fn new() -> Model{
        Model{
            patch:Patch::new(),
        }
    }
}


enum Message {
    New,
    AddToSelection(&mut Fixture),
    ClearSelection,
    SetMode(FixtureMode),
    Save,
    Load,

    Compose,
    SetSignalSource(SignalSource),
    SetTransition(Transition),

    Quit,
}

fn view(model: &Model, stdin: &mut termion::AsyncReader, stdout: &mut termion::raw::RawTerminal<io::Stdout>, dmx_port: &mut Box<dyn DmxPort> ) -> Option<Message>{
    
    //Terminal
    write!(stdout, "{}{}", termion::clear::All, termion::cursor::Goto(1, 1)).unwrap();
    write!(stdout, "Hello").unwrap();
    stdout.flush().unwrap();

    //DMX
    dmx_port.write(&[50,model.counter as u8,model.counter as u8][..]).unwrap();

    //Projectors

    if let Some(key) = stdin.keys().next() {
        match key.unwrap() {
            Key::Char('+') => return Some(Message::Increment),
            Key::Char('-') => return Some(Message::Decrement),
            Key::Char('q') => return Some(Message::Quit),
            _ => {}
        }
    }
    None
}

fn update(model: &mut Model, message: Message){
    match message {
        Message::Increment => model.counter += 1,
        Message::Decrement => model.counter -= 1,
        Message::Quit => (),
    }
}

fn main() {

    let mut stdin = termion::async_stdin();
    let mut stdout = io::stdout().into_raw_mode().unwrap();

    let mut dmx_port = &mut available_ports().unwrap()[1];
    dmx_port.open().unwrap();

    let frame_duration = Duration::from_secs_f64(1.0 / 30.0); // 30 FPS

    let mut model = Model::new();

    loop {
        let frame_start = Instant::now();
        
        if let Some(msg) = view(&model, &mut stdin, &mut stdout, &mut dmx_port) {
            if let Message::Quit = msg {
                dmx_port.close();
                break;
            }
            update(&mut model, msg);
        }

        let elapsed = frame_start.elapsed();
        if elapsed < frame_duration {
            thread::sleep(frame_duration - elapsed);
        }
    }
}
