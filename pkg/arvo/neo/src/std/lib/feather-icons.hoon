|%
++  svg
  |=  [viewbox=tape body=manx]
  ;svg
    =xmlns  "http://www.w3.org/2000/svg"
    =viewBox  viewbox
    =width  "1em"
    =fill  "currentColor"
    =style  "display: flex;"
    ;+  body
  ==
++  chevron-left
  %+  svg  "0 -960 960 960"
  ;path
    =d  "M560-240 320-480l240-240 56 56-184 184 184 184-56 56Z"
    ;
  ==
++  chevron-right
  %+  svg  "0 -960 960 960"
  ;path
    =d  "M504-480 320-664l56-56 240 240-240 240-56-56 184-184Z"
    ;
  ==
++  close
  %+  svg  "0 -960 960 960"
  ;path
    =d  "m256-200-56-56 224-224-224-224 56-56 224 224 224-224 56 56-224 224 224 224-56 56-224-224-224 224Z"
    ;
  ==
++  outline
  %+  svg  "0 -960 960 960"
  ;path
    =d  "M120-240v-80h240v80H120Zm0-200v-80h480v80H120Zm0-200v-80h720v80H120Z"
    ;
  ==
++  minimize
  %+  svg  "0 -960 960 960"
  ;path
    =d  "M240-120v-80h480v80H240Z"
    ;
  ==
++  loading
  %+  svg  "0 0 44 44"
  ;g(fill "none", fill-rule "evenodd", stroke-width "2", stroke "currentColor")
    ;circle(cx "22", cy "22", r "1")
      ;animate(attributeName "r", begin "0s", dur "1.8s", values "1; 20", calcMode "spline", keyTimes "0; 1", keySplines "0.165, 0.84, 0.44, 1", repeatCount "indefinite");
      ;animate(attributeName "stroke-opacity", begin "0s", dur "1.8s", values "1; 0", calcMode "spline", keyTimes "0; 1", keySplines "0.3, 0.61, 0.355, 1", repeatCount "indefinite");
    ==
    ;circle(cx "22", cy "22", r "1")
      ;animate(attributeName "r", begin "-0.9s", dur "1.8s", values "1; 20", calcMode "spline", keyTimes "0; 1", keySplines "0.165, 0.84, 0.44, 1", repeatCount "indefinite");
      ;animate(attributeName "stroke-opacity", begin "-0.9s", dur "1.8s", values "1; 0", calcMode "spline", keyTimes "0; 1", keySplines "0.3, 0.61, 0.355, 1", repeatCount "indefinite");
    ==
  ==
++  add
  %+  svg  "0 -960 960 960"
  ;path
    =d  "M440-440H200v-80h240v-240h80v240h240v80H520v240h-80v-240Z"
    ;
  ==
--
