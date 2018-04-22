cd House_Management.images/
${DOT:-dot} -Tpng -oHouse_Management.overall.png -Tcmapx -oHouse_Management.overall.cmapx House_Management.overall.dot
${CIRCO:-circo} -Tpng -oHouse_Management.A1.class.png -Tcmapx -oHouse_Management.A1.class.cmapx House_Management.A1.class.dot
${CIRCO:-circo} -Tpng -oHouse_Management.Button.class.png -Tcmapx -oHouse_Management.Button.class.cmapx House_Management.Button.class.dot
${CIRCO:-circo} -Tpng -oHouse_Management.Lamp.class.png -Tcmapx -oHouse_Management.Lamp.class.cmapx House_Management.Lamp.class.dot
${CIRCO:-circo} -Tpng -oHouse_Management.Timed_Button.class.png -Tcmapx -oHouse_Management.Timed_Button.class.cmapx House_Management.Timed_Button.class.dot
${DOT:-dot} -Tpng -oHouse_Management.Timed_Button.state.png House_Management.Timed_Button.state.dot
