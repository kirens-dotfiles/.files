class Keymap {
  constructor({
    keycodes = '',
    types = '',
    compatibility = '',
    symbols = '',
    geometry = '',
  }) {
    this.keycodes = keycodes
    this.types = types
    this.compatibility = compatibility
    this.symbols = symbols
    this.geometry = geometry
  }
  toString(indent = '', step = '  ') {
    const newIndt = indent + step
    return '' +
      'xkb_keymap {\n' +
      this.keycodes.toString(newIndt, step) +
      this.types.toString(newIndt, step) +
      this.compatibility.toString(newIndt, step) +
      this.symbols.toString(newIndt, step) +
      this.geometry.toString(newIndt, step) +
      '}\n'
  }
}

Object.entries = (obj) => {
  const entries = []
  for(let key in obj) entries.push([key, obj[key]])

  return entries
}

const mapOn
  = array => fn =>
    array.map(fn)
const mapOnJoin
  = array => joiner => fn =>
    mapOn(array)(fn).join(joiner)
const mapOnJoinTrail
  = array => joiner => fn =>
    mapOnJoin(array)(joiner)(fn) + joiner
const paddingWith
  = pad => aim => text =>
    pad.repeat(Math.max(0, aim-text.toString().length))
const padWith
  = pad => aim => text =>
    paddingWith(pad)(aim)(text) + text
const fillSpaces
  = (name, aim) =>
    name + paddingWith(' ')(aim)(name)

Array.prototype.flatten = function(lvl, flattenInto = []) {
  return lvl < 0
    ? (flattenInto.push(this), flattenInto)
    : this.reduce((_, el) =>
        Array.isArray(el)
        ? el.flatten(lvl - 1, flattenInto)
        : (flattenInto.push(el), flattenInto)
      , flattenInto)
}

// type alias KeyCodeRec
//   { dev: String
//   , name: String
//   , min: int
//   , max: int
//   , basic: Map String Int
//   , aliases: Map String String
//   , leds: Map Int {isVirtual: ?Bool, name: String}
//   }
class Keycodes {
  // KeyCodeRec -> Keycode
  constructor({dev = 'evdev', name = 'QWERTY', min, max, basic, aliases, leds}) {
    this.dev = dev
    this.name = name
    this.minimum = min
    this.maximum = max
    this.basic = basic
    this.aliases = aliases
    this.leds = leds
  }
  toString(indent = '', step = '  ') {
    const newIndt = indent + step
    const basic = mapOnJoinTrail([...this.basic.entries()].sort((a,b) => a[1] - b[1]))('\n')
    const alias = mapOnJoinTrail([...this.aliases.entries()])('\n')
    const led = mapOnJoinTrail([...this.leds.entries()])('\n')

    return '' +
      `${indent}xkb_keycodes "${this.dev}+aliases(${this.name})" {\n` +
        `${newIndt}minimum = ${this.minimum};\n` +
        `${newIndt}maximum = ${this.maximum};\n` +
        basic(([name, key]) =>
          `${newIndt}<${fillSpaces(name + '>', 4)} = ${key}`
        ) +
        led(([code, {isVirtual, name}]) =>
          `${newIndt}${isVirtual ? 'virtual ' : ''}indicator ${code} = "${name}"`
        ) +
        alias(([alias, name]) =>
          `${newIndt}alias <${fillSpaces(alias, 4)}> = <${name}>`
        ) +
      `${indent}};\n`
  }
}

// type alias mappings
//   List [List String, String, ?List String] -- modifiers -> (Levels, preserves)
class Type {
  constructor(mappings) {
    this.allModifiers = new Set()
    const levels = this.levels = new Map([
      [mappings.length ? 'Base' : 'Any','Level1']
    ])

    this.mappings = mappings.map(([modifiers, level, preserve]) => {
      modifiers.map(m => this.allModifiers.add(m))

      if(!levels.has(level))
        levels.set(level, (levels.size < 8 ? 'Level' : '') + (levels.size + 1))

      return [modifiers, this.levels.get(level), preserve]
    })
  }
  toString(indent = '', step = '  ', name = this.name) {
    const newIndt = indent + step
    const mappings = mapOnJoinTrail(this.mappings)('\n')
    const levels = mapOnJoinTrail([...this.levels.entries()])('\n')
    return '' +
      `${indent}type "${name}" {\n` +
        `${newIndt}modifiers = ${[...this.allModifiers].join('+') || 'none'}\n` +
        mappings(([modifiers, level, preserve]) =>
          `${newIndt}map[${modifiers.join('+')}] = ${level}` +
          (preserve
            ? `\n${newIndt}preserve[${modifiers.join('+')}] = ${preserve.join('+')}`
            : ''
          )
        ) +
        levels(([name, level]) => `${newIndt}level_name[${level}] = "${name}"`) +
      `${indent}};\n`
  }
}

// type alias TypesRec
//   { types: Map String Type
//   , modifiers: Set String
//   }
class Types {
  constructor({types, modifiers}) {
    this.types = types
    this.modifiers = modifiers
  }
  toString(indent = '', step = '  ') {
    const newIndt = indent + step
    const types = mapOnJoinTrail([...this.types.entries()])('')

    return '' +
      `${indent}xkb_types "complete" {\n` +
        `${newIndt}virtual_modifiers ${[...this.modifiers.entries()].join(',')}\n\n` +
        types(([name, type]) => type.toString(newIndt, step, name)) +
      `${indent}};\n`

  }
}

class Compatibility {
  constructor() {

  }
  toString(indent = '', step = '  ') {

  }
}

// TODO: multiple groups
class Symbols {
  constructor({dev = 'evdev', lang = 'en', language = 'English', extra = ''}) {

  }
  toString(indent = '', step = '  ') {
    const newIndt = indent + step

    return '' +
      `${indent}xkb_symbols "pc+${this.lang}+inet(${dev})+terminate(ctrl_alt_bksp)" {\n` +
      `${newIndent}name[group1]="${this.language}"\n` +
      `${indent}};\n`
  }
}

class Geometry {
  constructor() {

  }
  toString(indent = '', step = '  ') {

  }
}



const countUp
  = (min, max) =>
    Array.from(Array(max-min))
      .map((_, i) => min+i)

const countUpIncluding
  = (min, max) =>
    countUp(min, max+1)

const num2
  = padWith('0')(2)
const num3
  = padWith('0')(3)

const rowNames = ['AA', 'AB', 'AC', 'AD', 'AE', 'AF', 'AG']

const myKeyCodes = new Keycodes({
  min: 8,
  max: 255,
  basic: new Map([
    // Keyboard mapping
    ...[
      // Top row
      [9, ...countUpIncluding(67, 76), 95, 96, 107, 119],
      // Number row
      [49, ...countUpIncluding(10, 22)],
      // Upper row
      [...countUpIncluding(23, 35), 51],
      // Home row
      [66, ...countUpIncluding(38, 48), 36],
      // Lower row
      [50, ...countUpIncluding(52, 62)],
      // Bottom row
      [37, 133, 64, 65, 108, 135, 94],
    ].map((row, rowNum) =>
      row.map((key, idx) => [rowNames[rowNum] + num2(idx), key])
    ).flatten(1),
    // Other characters
    ...Object.entries({
      LEFT: 113, DOWN: 116, UP: 111, RIGHT: 114,
      HOME: 110, PGDN: 117, PGUP: 112, END: 115,
      INS: 118,
      MUTE: 121, 'VOL-': 122, 'VOL+': 123,
      PREV: 173, NEXT: 171, PLPS: 172,
      FLGT: 255,
      LGTU: 233, LGTD: 232,
      POWR: 124,
    }).map(([a, b]) => [b, a]),
    // Non-existent
    ...[
      [63, 109, 120, 134],
      countUpIncluding(77, 92),
      countUpIncluding(97, 106),
      countUpIncluding(125, 132),
      countUpIncluding(125, 132),
      countUpIncluding(136, 170),
      countUpIncluding(174, 231),
      countUpIncluding(234, 253),
    ].map(r => r.map(n => ['unkn', n]))
     .flatten(1)
  ]),
  leds: new Map([
    'Caps Lock',
    'Num Lock',
    'Scroll Lock',
    'Compose',
    'Kana',
    'Sleep',
    'Suspend',
    'Mute',
    'Misc',
    'Mail',
    'Charging',
    ['Shift Lock'],
    ['Group 2'],
    ['Mouse Keys'],
  ].map((n, i) =>
    [i+1, Array.isArray(n) ? {name: n[0], isVirtual: true} : {name: n}]
  )),
  aliases: new Map([...Object.entries({
    AC12: 'BKSL',
    MENU: 'COMP',
    HZTG: 'TLDE',
    LMTA: 'LWIN',
    RMTA: 'RWIN',
    ALGR: 'RALT',
    KPPT: 'I129',
    LatQ: 'AD01',
    LatW: 'AD02',
    LatE: 'AD03',
    LatR: 'AD04',
    LatT: 'AD05',
    LatY: 'AD06',
    LatU: 'AD07',
    LatI: 'AD08',
    LatO: 'AD09',
    LatP: 'AD10',
    LatA: 'AC01',
    LatS: 'AC02',
    LatD: 'AC03',
    LatF: 'AC04',
    LatG: 'AC05',
    LatH: 'AC06',
    LatJ: 'AC07',
    LatK: 'AC08',
    LatL: 'AC09',
    LatZ: 'AB01',
    LatX: 'AB02',
    LatC: 'AB03',
    LatV: 'AB04',
    LatB: 'AB05',
    LatN: 'AB06',
    LatM: 'AB07',
  })]),
})

const mySymbols = new Symbols({
  lang: 'sv', language: 'Swedish', extra: '+terminate(ctrl_alt_bksp)',

})

const myTypes = new Types({
  modifiers: new Set(['NumLock','Alt','LevelThree','LAlt','RAlt','RControl','LControl','ScrollLock','LevelFive','AltGr','Meta','Super','Hyper']),
  types: new Map(Object.entries({//TODO: lvl1, no modifiers
    ONE_LEVEL: new Type([]),
    TWO_LEVEL: new Type([
      [['Shift'], 'Shift'],
    ]),
    ALPHABETIC: new Type([
      [['Shift'], 'Caps'],
      [['Lock'], 'Caps'],
    ]),
    KEYPAD: new Type([
      [['Shift'], 'Number'],
      [['NumLock'], 'Number'],
    ]),
    'SHIFT+ALT': new Type([
      [['Shift'], 'Shift+Alt'],
      [['NumLock'], 'Shift+Alt'],
    ]),
    'CTRL+ALT': new Type([
      [['Shift'], 'Shift', ['Shift']],
      [['LevelThree'], 'Alt Base'],
      [['Shift', 'LevelThree'], 'Shift Alt', ['Shift']],
      [['Control', 'Akt'], 'Ctrl+Alt'],
    ]),
  })),
})

console.log(new Keymap({
  keycodes: myKeyCodes,
  types: myTypes,
}).toString())
