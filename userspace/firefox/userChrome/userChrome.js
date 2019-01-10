console.log('Running custom userChrome script, V 0.1.2')

;
[ { selector: '#key_quitApplication'
  , msg: 'Will remove Ctrl+Q quit application shortcut'
  }
, { selector: '*[key="d"][command="Browser:AddBookmarkAs"]'
  , msg: 'Will remove Ctrl+D bookmark'
  }
, { selector: '*[key="W"][command="cmd_close"]'
  , msg: 'Will remap Ctrl+W close tab shortcut to Ctrl+D'
  , remap: 'D'
  }
, { selector: '*[key="W"][command="cmd_closeWindow"]'
  , msg: 'Will remove Ctrl+Shift+W close tab shortcut'
  }
].map(({ selector, msg, remap }) => {
  console.info(msg)
  if (remap) {
    document.querySelector(selector).setAttribute('key', remap)
  } else {
    document.querySelector(selector).remove()
  }
})
