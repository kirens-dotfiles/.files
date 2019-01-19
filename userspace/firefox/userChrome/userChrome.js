console.log('Running custom userChrome script, V 0.2.1')

;
[ { selector: '#key_quitApplication'
  , msg: 'Will remove Ctrl+Q quit application shortcut'
  }
, { selector: '*[key="d"][command="Browser:AddBookmarkAs"]'
  , msg: 'Will remove Ctrl+D bookmark'
  }
, { selector: '*[key="W"][command="cmd_close"]'
  , msg: 'Will remap Ctrl+W close tab shortcut to Ctrl+D'
  , remap: { modifiers: 'accel,shift', key: 'D' }
  }
, { selector: '*[key="W"][command="cmd_closeWindow"]'
  , msg: 'Will remove Ctrl+Shift+W close tab shortcut'
  }
].map(({ selector, msg, remap }) => {
  console.info(msg)
  const keyBind = document.querySelector(selector);
  if (remap) {
    keyBind.setAttribute('key', remap.key)
    keyBind.setAttribute('modifiers', remap.modifiers)
  } else {
    keyBind.remove()
  }
})
