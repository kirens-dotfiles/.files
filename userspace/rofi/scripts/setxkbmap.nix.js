{ rofi, node, setxkbmap }:
''
#! ${node}
const { spawn } = require('child_process')

const cmd = ({ bin, data = ''', args = [] }) => new Promise((resolve, reject) => {
  const rofi = spawn(bin, args)
  rofi.stdin.write(data)
  rofi.stdin.end()
  let result = ''';
  let err = ''';
  rofi.stdout.on('data', data => { result += data.toString() })
  rofi.stderr.on('data', data => { err += data.toString() })
  rofi.on('close', code => code ? reject({code, err}) : resolve(result))
})

const init = async () => {
  const layout = await cmd({
    bin: '${rofi}',
    args: ['-dmenu', '-p', 'Input layout:']
  });

  await cmd({ bin: '${setxkbmap}', args: layout.split(' ') });
}

init()
  .catch(err => console.error(err) || process.exit(1))
''
