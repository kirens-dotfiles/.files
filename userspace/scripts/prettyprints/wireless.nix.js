{ nodejs-slim-10_x, wirelesstools }:
let
  iwconfig = "${wirelesstools}/bin/iwconfig";
in ''
#! ${nodejs-slim-10_x}/bin/node

const network = raw => {
  const match = (/ESSID:("?)(.*?)(\\x00)?\1(?=\s*\n)/g).exec(raw)
  if (!match) return { error: "NO_MATCH", name: "ERROR" }

  return { name: match[2], regularName: !!match[1] }
}

const between = (min, num, max) => Math.min(max, Math.max(min, num))

const strength = raw => {
  const match = (/Quality=(\d{1,2})\/(\d{1,2})/).exec(raw)
  if (!match) return -1

  return between(0, Math.floor(match[1]/match[2] * 10), 10)
}

require('child_process').exec('${iwconfig} wlo1', (error, stdout) => {
  const { name, regularName } = network(stdout)
  const signal = strength(stdout)

  const bar = isNaN(signal)
    ? '          '
    : signal === -1
    ? '----!!----'
    : '/'.repeat(signal) + '-'.repeat(10-signal)

  console.log(
    (error ? '¡' : ''')
    + (regularName ? ''' : '!')
    + name
    + ` [''${bar}]`
  )
})
''
