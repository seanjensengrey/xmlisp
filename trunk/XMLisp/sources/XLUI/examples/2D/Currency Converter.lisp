;; Currency Converter, compare to http://trac.clozure.com/openmcl/wiki/EasyGuiCurrencyConverter
;; http://developer.apple.com/documentation/Cocoa/Conceptual/ObjCTutorial/02Essence/02Essence.html#//apple_ref/doc/uid/TP40000863-CH3-SW5
;; 02/04/09 Alexander Repenning

(in-package :xlui)


(defmethod CONVERT-CURRENCY-ACTION ((w application-window) (Button button))
  (setf (value (view-named w "amount"))
        (* (value (view-named w "rate"))
           (value (view-named w "dollars")))))


<application-window title="Currency Converter" width="300" height="180">
  <column align="stretch" valign="stretch" padding="9">
    <row align="stretch" minimize="vertical" valign="bottom">
     <label text="Exchange Rate per $1:" align="right" flex="2"/>
     <editable-number name="rate" text="1.0" flex="1"/>
    </row>
    <row align="stretch" minimize="vertical" valign="bottom">
     <label text="Dollars to Convert:" align="right" flex="2"/>
     <editable-number name="dollars" text="1.0" flex="1"/>
    </row>
    <row align="stretch" minimize="vertical" valign="bottom">
     <label text="Amount in the other Currency:" align="right" flex="2"/>
     <editable-number  name="amount" text="1.0" flex="1"/>
    </row>
    <row align="right" valign="bottom" vflex="1">
      <button text="Convert" action="convert-currency-action" default-button="true"/>
    </row>
  </column>
</application-window>
