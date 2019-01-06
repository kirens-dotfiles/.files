console.log('Running custom userChrome script')

;[ { selector: '#key_quitApplication'
  , msg: 'Will remove Ctrl+Q quit application shortcut'
  }
, { selector: '*[key="W"][command="cmd_close"]'
  , msg: 'Will remove Ctrl+W close tab shortcut'
  }
, { selector: '*[key="W"][command="cmd_closeWindow"]'
  , msg: 'Will remove Ctrl+W close tab shortcut'
  }
].map(({ selector, msg }) => {
  console.info(msg)
  document.querySelector(selector).remove()
})
