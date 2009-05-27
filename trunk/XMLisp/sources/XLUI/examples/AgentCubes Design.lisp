

(in-package :xlui)


(defclass INCLUDE (xml-serializer)
  ((src :accessor src :documentation "Path to file to include"))
  (:documentation "include a file"))


(defmethod READ-RETURN-VALUE ((Self include))
  (load-object (src Self)))


<application-window title="AgentCubes" width="1100" height="700" margin="0">
  <row align="stretch" valign="stretch">
   <column align="stretch" valign="stretch" width="200" src="sidebar_texture.png">
    <row align="center" valign="middle" height="30" src="name_bar_background.png">
      <label text="project" width="60"/>
    </row>
    <scroll-box vflex="1" has-horizontal-scroller="false">
      <include src="/Users/alex/Documents/products/sources/XLUI/examples/filelist.xml"/>
    </scroll-box>
  </column>
  <column align="stretch" valign="stretch" flex="1">
    <row align="center" valign="middle" height="30" src="name_bar_background.png">
      <img src="agentcubes/world_toolbar.png"/>
    </row>
    <img src="agentcubes/frogger_world.png" vflex="1"/>
    <row align="stretch" valign="stretch" vflex="1">
    <column align="stretch" valign="stretch" flex="1" vflex="1">
      <row align="center" valign="middle" height="30" src="name_bar_background.png">
        <label text="conditions" width="80"/>
      </row>
      <scroll-box flex="1" vflex="1">
       <img src="agentcubes/condition_pallette.png"/>
      </scroll-box>
    </column>
    <column align="stretch" valign="stretch" flex="2" vflex="1">
      <row align="center" valign="middle" height="30" src="name_bar_background.png">
        <label text="behavior: Frog" width="120"/>
      </row>
    <img src="agentcubes/cover_flow_behaviors.png" flex="2" vflex="1"/>  
    </column>
    <column align="stretch" valign="stretch" flex="1" vflex="1">
      <row align="center" valign="middle" height="30" src="name_bar_background.png">
        <label text="actions" width="80"/>
      </row>
    <scroll-box flex="1" vflex="1">
      <img src="agentcubes/action_pallette.png"/>
    </scroll-box>
    </column>
  </row>
  </column>
  </row>
</application-window>
