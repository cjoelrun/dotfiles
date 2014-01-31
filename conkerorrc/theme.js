register_user_stylesheet(
    make_css_data_uri(
        ["#minibuffer, .mode-line {background: #000; color: white; font: 9px Inconsolata;}"+
         "#minibuffer-prompt {color: #aaa;}"+
         "#tab2-bar {background: #000;}"+
         "#tab2-bar .tab2 {margin: 1px 0 0 1px; padding: 0; height: 19px; border: 1px solid #666; min-width: 100px; max-width: 120px; background: #333; overflow: hidden;} "+
         "#tab2-bar .tab2-label {margin: 0; color: black; font: 9px Inconsolata;}"+
         "#tab2-bar .tab2-icon {height: 10px; width: 16px; background: #222; color: white; border: 1px solid #999; font: 900 9px Inconsolata; text-align: center;}"+
         "#tab2-bar .tab2[selected=true] {border: 1px solid white; background: #666; border: 1px solid #999;}"+
         "select,option,input { color: #000 !important; background-color: #333 !important; }"+
         "code, pre { font -family: Inconsolata !important; "+
         "font-size: 9px !important; }"+
         "span.__conkeror_hint { line-height: 1.5ems !important; font-family: Inconsolata !important; padding: 3px !important; font-weight: normal !important; font-size: 9px !important; }"], $namespace = XUL_NS));
