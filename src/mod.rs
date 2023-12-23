//! Personal System/2 (PS/2) is a connector designed for keyboards and mouses.
//! It has now been deprecated in favor of USB keyboards/mouses.

#![no_std]
#![no_main]
#![feature(trait_upcasting)]

use core::any::Any;
use kernel::device::keyboard::Keyboard;
use kernel::device::keyboard::KeyboardAction;
use kernel::device::keyboard::KeyboardKey;
use kernel::device::keyboard::KeyboardLED;
use kernel::device::keyboard::KeyboardManager;
use kernel::device::manager;
use kernel::event;
use kernel::event::CallbackHook;
use kernel::event::CallbackResult;
use kernel::idt;
use kernel::io;
use kernel::println;
use kernel::process::regs::Regs;

kernel::module!([]);

/// The interrupt number for keyboard input events.
const KEYBOARD_INTERRUPT_ID: u32 = 0x21;

/// The PS/2 controller data port.
const DATA_REGISTER: u16 = 0x60;
/// The PS/2 controller status port.
const STATUS_REGISTER: u16 = 0x64;
/// The PS/2 controller status port.
const COMMAND_REGISTER: u16 = 0x64;

/// The maximum number of attempts for sending a command to the PS/2 controller.
const MAX_ATTEMPTS: usize = 3;

/// Response telling the test passed.
const TEST_CONTROLLER_PASS: u8 = 0x55;

/// Response telling the keyboard test passed.
const TEST_KEYBOARD_PASS: u8 = 0x00;

/// Command to set the keyboard's LEDs state.
const KEYBOARD_LED: u8 = 0xed;
/// Command to set the keyboard's scancode set.
const KEYBOARD_SCANCODE: u8 = 0xf0;
/// Command to set the keyboard's typematic byte.
const KEYBOARD_TYPEMATIC: u8 = 0xf3;
/// Command to enable keyboard scanning.
const KEYBOARD_ENABLE: u8 = 0xf4;

/// Keyboard acknowledgement.
const KEYBOARD_ACK: u8 = 0xfa;

// TODO Turn commands and flags into constants.

/// A slice containing a pair of keycode and enumeration that allows to associate a keycode with
/// its enumeration entry.
static NORMAL_KEYS: [(u8, KeyboardKey); 85] = [
    (0x01, KeyboardKey::KeyF9),
    (0x03, KeyboardKey::KeyF5),
    (0x04, KeyboardKey::KeyF3),
    (0x05, KeyboardKey::KeyF1),
    (0x06, KeyboardKey::KeyF2),
    (0x07, KeyboardKey::KeyF12),
    (0x09, KeyboardKey::KeyF10),
    (0x0a, KeyboardKey::KeyF8),
    (0x0b, KeyboardKey::KeyF6),
    (0x0c, KeyboardKey::KeyF4),
    (0x0d, KeyboardKey::KeyTab),
    (0x0e, KeyboardKey::KeyBackTick),
    (0x11, KeyboardKey::KeyLeftAlt),
    (0x12, KeyboardKey::KeyLeftShift),
    (0x14, KeyboardKey::KeyLeftControl),
    (0x15, KeyboardKey::KeyQ),
    (0x16, KeyboardKey::Key1),
    (0x1a, KeyboardKey::KeyZ),
    (0x1b, KeyboardKey::KeyS),
    (0x1c, KeyboardKey::KeyA),
    (0x1d, KeyboardKey::KeyW),
    (0x1e, KeyboardKey::Key2),
    (0x21, KeyboardKey::KeyC),
    (0x22, KeyboardKey::KeyX),
    (0x23, KeyboardKey::KeyD),
    (0x24, KeyboardKey::KeyE),
    (0x25, KeyboardKey::Key4),
    (0x26, KeyboardKey::Key3),
    (0x29, KeyboardKey::KeySpace),
    (0x2a, KeyboardKey::KeyV),
    (0x2b, KeyboardKey::KeyF),
    (0x2c, KeyboardKey::KeyT),
    (0x2d, KeyboardKey::KeyR),
    (0x2e, KeyboardKey::Key5),
    (0x31, KeyboardKey::KeyN),
    (0x32, KeyboardKey::KeyB),
    (0x33, KeyboardKey::KeyH),
    (0x34, KeyboardKey::KeyG),
    (0x35, KeyboardKey::KeyY),
    (0x36, KeyboardKey::Key6),
    (0x3a, KeyboardKey::KeyM),
    (0x3b, KeyboardKey::KeyJ),
    (0x3c, KeyboardKey::KeyU),
    (0x3d, KeyboardKey::Key7),
    (0x3e, KeyboardKey::Key8),
    (0x41, KeyboardKey::KeyComma),
    (0x42, KeyboardKey::KeyK),
    (0x43, KeyboardKey::KeyI),
    (0x44, KeyboardKey::KeyO),
    (0x45, KeyboardKey::Key0),
    (0x46, KeyboardKey::Key9),
    (0x49, KeyboardKey::KeyDot),
    (0x4a, KeyboardKey::KeySlash),
    (0x4b, KeyboardKey::KeyL),
    (0x4c, KeyboardKey::KeySemiColon),
    (0x4d, KeyboardKey::KeyP),
    (0x4e, KeyboardKey::KeyMinus),
    (0x52, KeyboardKey::KeySingleQuote),
    (0x54, KeyboardKey::KeyOpenBrace),
    (0x55, KeyboardKey::KeyEqual),
    (0x58, KeyboardKey::KeyCapsLock),
    (0x59, KeyboardKey::KeyRightShift),
    (0x5a, KeyboardKey::KeyEnter),
    (0x5b, KeyboardKey::KeyCloseBrace),
    (0x5d, KeyboardKey::KeySlash),
    (0x66, KeyboardKey::KeyBackspace),
    (0x69, KeyboardKey::KeyKeypad1),
    (0x6b, KeyboardKey::KeyKeypad4),
    (0x6c, KeyboardKey::KeyKeypad7),
    (0x70, KeyboardKey::KeyKeypad0),
    (0x71, KeyboardKey::KeyKeypadDot),
    (0x72, KeyboardKey::KeyKeypad2),
    (0x73, KeyboardKey::KeyKeypad5),
    (0x74, KeyboardKey::KeyKeypad6),
    (0x75, KeyboardKey::KeyKeypad8),
    (0x76, KeyboardKey::KeyEsc),
    (0x77, KeyboardKey::KeyNumberLock),
    (0x78, KeyboardKey::KeyF11),
    (0x79, KeyboardKey::KeyKeypadPlus),
    (0x7a, KeyboardKey::KeyKeypad3),
    (0x7b, KeyboardKey::KeyKeypadMinus),
    (0x7c, KeyboardKey::KeyKeypadStar),
    (0x7d, KeyboardKey::KeyKeypad9),
    (0x7e, KeyboardKey::KeyScrollLock),
    (0x83, KeyboardKey::KeyF7),
];

/// Same as `NORMAL_KEYS` except this slice stores keys beginning with `0xE0`.
static SPECIAL_KEYS: [(u8, KeyboardKey); 38] = [
    (0x10, KeyboardKey::KeyWWWSearch),
    (0x11, KeyboardKey::KeyRightAlt),
    (0x14, KeyboardKey::KeyRightControl),
    (0x15, KeyboardKey::KeyPreviousTrack),
    (0x18, KeyboardKey::KeyWWWFavorites),
    (0x1f, KeyboardKey::KeyLeftGUI),
    (0x20, KeyboardKey::KeyWWWRefresh),
    (0x21, KeyboardKey::KeyVolumeDown),
    (0x23, KeyboardKey::KeyMute),
    (0x27, KeyboardKey::KeyRightGUI),
    (0x28, KeyboardKey::KeyWWWStop),
    (0x2b, KeyboardKey::KeyCalculator),
    (0x2f, KeyboardKey::KeyApps),
    (0x30, KeyboardKey::KeyWWWForward),
    (0x32, KeyboardKey::KeyVolumeUp),
    (0x34, KeyboardKey::KeyPlay),
    (0x37, KeyboardKey::KeyACPIPower),
    (0x38, KeyboardKey::KeyWWWBack),
    (0x3a, KeyboardKey::KeyWWWHome),
    (0x3b, KeyboardKey::KeyStop),
    (0x3f, KeyboardKey::KeyACPISleep),
    (0x40, KeyboardKey::KeyMyComputer),
    (0x48, KeyboardKey::KeyEmail),
    (0x4a, KeyboardKey::KeyKeypadSlash),
    (0x4d, KeyboardKey::KeyNextTrack),
    (0x50, KeyboardKey::KeyMediaSelect),
    (0x5a, KeyboardKey::KeyEnter),
    (0x5e, KeyboardKey::KeyACPIWake),
    (0x69, KeyboardKey::KeyEnd),
    (0x6b, KeyboardKey::KeyCursorLeft),
    (0x6c, KeyboardKey::KeyHome),
    (0x70, KeyboardKey::KeyInsert),
    (0x71, KeyboardKey::KeyDelete),
    (0x72, KeyboardKey::KeyCursorDown),
    (0x74, KeyboardKey::KeyCursorRight),
    (0x75, KeyboardKey::KeyCursorUp),
    (0x7a, KeyboardKey::KeyPageDown),
    (0x7d, KeyboardKey::KeyPageUp),
];

/// Tells whether the PS/2 buffer is ready for reading.
fn can_read() -> bool {
    unsafe { io::inb(STATUS_REGISTER) & 0b1 != 0 }
}

/// Tells whether the PS/2 buffer is ready for writing.
fn can_write() -> bool {
    unsafe { io::inb(STATUS_REGISTER) & 0b10 == 0 }
}

/// Waits until the buffer is ready for reading.
fn wait_read() {
    while !can_read() {}
}

/// Waits until the buffer is ready for reading.
fn wait_write() {
    while !can_write() {}
}

/// Clears the PS/2 controller's buffer.
fn clear_buffer() {
    while can_read() {
        unsafe {
            io::inb(DATA_REGISTER);
        }
    }
}

/// Sends the given data `data` to the keyboard.
fn keyboard_send(data: u8) -> Result<(), ()> {
    let mut response = 0;

    for _ in 0..MAX_ATTEMPTS {
        wait_write();
        unsafe {
            io::outb(DATA_REGISTER, data);
        }

        wait_read();
        response = unsafe { io::inb(DATA_REGISTER) };
        if response == KEYBOARD_ACK {
            return Ok(());
        }
    }

    if response == KEYBOARD_ACK {
        Ok(())
    } else {
        Err(())
    }
}

/// Sends the given command `command` and returns the response.
fn send_command(command: u8, expected_response: u8) -> Result<(), ()> {
    for _ in 0..MAX_ATTEMPTS {
        wait_write();
        unsafe {
            io::outb(COMMAND_REGISTER, command);
        }

        wait_read();
        let response = unsafe { io::inb(DATA_REGISTER) };
        if response == expected_response {
            return Ok(());
        }
    }
    Err(())
}

/// Disables PS/2 devices.
fn disable_devices() {
    wait_write();
    unsafe {
        io::outb(COMMAND_REGISTER, 0xad);
    }

    wait_write();
    unsafe {
        io::outb(COMMAND_REGISTER, 0xa7);
    }
}

/// Enables the keyboard device.
fn enable_keyboard() -> Result<(), ()> {
    wait_write();
    unsafe {
        io::outb(COMMAND_REGISTER, 0xae);
    }

    // Set the keyboard's LEDs
    keyboard_send(KEYBOARD_LED)?;
    keyboard_send(0)?;

    // Set keyboard's scancode set
    keyboard_send(KEYBOARD_SCANCODE)?;
    keyboard_send(2)?;

    // Set keyboard's typematic byte
    keyboard_send(KEYBOARD_TYPEMATIC)?;
    keyboard_send(0)?;

    // Enable keyboard scanning
    keyboard_send(KEYBOARD_ENABLE)?;

    Ok(())
}

/// Returns the configuration byte.
fn get_config_byte() -> u8 {
    wait_write();
    unsafe {
        io::outb(COMMAND_REGISTER, 0x20);
    }

    wait_read();
    unsafe { io::inb(DATA_REGISTER) }
}

/// Sets the configuration byte.
fn set_config_byte(config: u8) {
    wait_write();
    unsafe {
        io::outb(COMMAND_REGISTER, 0x60);
    }

    wait_write();
    unsafe {
        io::outb(DATA_REGISTER, config);
    }
}

/// Tests the PS/2 controller.
fn test_controller() -> Result<(), ()> {
    send_command(0xaa, TEST_CONTROLLER_PASS)
}

/// Tests the first device.
fn test_device() -> Result<(), ()> {
    send_command(0xab, TEST_KEYBOARD_PASS)
}

/// Reads one byte of keycode from the controller.
fn read_keycode_byte() -> u8 {
    wait_read();
    unsafe { io::inb(DATA_REGISTER) }
}

/// Reads a keystroke and returns the associated key and action.
fn read_keystroke() -> (KeyboardKey, KeyboardAction) {
    let mut keycode = read_keycode_byte();
    let special = keycode == 0xe0;
    if special {
        keycode = read_keycode_byte();
    }
    let action = if keycode == 0xf0 {
        keycode = read_keycode_byte();
        KeyboardAction::Released
    } else {
        KeyboardAction::Pressed
    };
    // TODO Add support for print screen and pause

    let list = if !special {
        &NORMAL_KEYS[..]
    } else {
        &SPECIAL_KEYS[..]
    };
    let index = list.binary_search_by(|k: &(u8, KeyboardKey)| k.0.cmp(&keycode));
    let key = if let Ok(i) = index {
        list[i].1
    } else {
        KeyboardKey::KeyUnknown
    };

    (key, action)
}

/// Handles the given keyboard input.
///
/// Arguments:
/// - `key` is the key that has been typed.
/// - `action` is the action.
fn handle_input(key: KeyboardKey, action: KeyboardAction) {
    // TODO Do not retrieve at each keystroke
    let Some(manager_mutex) = manager::get::<KeyboardManager>() else {
        return;
    };
    let mut manager = manager_mutex.lock();
    let kbd_manager = (&mut *manager as &mut dyn Any)
        .downcast_mut::<KeyboardManager>()
        .unwrap();
    kbd_manager.input(key, action);
}

/// Global variable containing the module's instance.
static mut PS2_KEYBOAD: Option<PS2Keyboard> = None;

/// The PS2 keyboard structure.
pub struct PS2Keyboard {
    /// The callback hook for keyboard input interrupts.
    keyboard_interrupt_callback_hook: Option<CallbackHook>,

    /// The state of LEDs.
    leds_state: u8,
}

impl PS2Keyboard {
    /// Creates the keyboard's instance.
    pub fn new() -> Result<Self, ()> {
        let mut s = Self {
            keyboard_interrupt_callback_hook: None,

            leds_state: 0,
        };
        s.init()?;
        Ok(s)
    }

    /// Initializes the handler.
    fn init(&mut self) -> Result<(), ()> {
        // TODO Check if PS/2 controller exists using ACPI

        idt::wrap_disable_interrupts(|| {
            disable_devices();
            clear_buffer();

            set_config_byte(get_config_byte() & 0b10111100);

            test_controller()?;
            test_device()?;
            enable_keyboard()?;

            set_config_byte(get_config_byte() | 0b1);

            Ok(())
        })?;

        let callback = |_id: u32, _code: u32, _regs: &Regs, _ring: u32| {
            while can_read() {
                let (key, action) = read_keystroke();
                handle_input(key, action);
            }

            CallbackResult::Continue
        };

        let hook_result = event::register_callback(KEYBOARD_INTERRUPT_ID, callback);
        self.keyboard_interrupt_callback_hook = hook_result.map_err(|_| ())?;

        clear_buffer();
        Ok(())
    }
}

impl Keyboard for PS2Keyboard {
    fn set_led(&mut self, led: KeyboardLED, enabled: bool) {
        let offset = match led {
            KeyboardLED::ScrollLock => 0,
            KeyboardLED::NumberLock => 1,
            KeyboardLED::CapsLock => 2,
        };

        if enabled {
            self.leds_state |= 1 << offset;
        } else {
            self.leds_state &= !(1 << offset);
        }

        let _ = keyboard_send(KEYBOARD_LED);
        let _ = keyboard_send(self.leds_state);
    }
}

#[no_mangle]
pub extern "C" fn init() -> bool {
    match PS2Keyboard::new() {
        Ok(kbd) => {
            unsafe {
                // Safe because only one thread can access this function
                PS2_KEYBOAD = Some(kbd);
            }
            true
        }
        Err(()) => {
            println!("Failed to initialize PS2 keyboard!");
            false
        }
    }
}

#[no_mangle]
pub extern "C" fn fini() {
    unsafe {
        // Safe because only one thread can access this function
        PS2_KEYBOAD = None;
    }
}
