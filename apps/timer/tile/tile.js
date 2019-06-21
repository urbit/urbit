import React, { Component } from 'react';
import classnames from 'classnames';

import api from './api';
import VolumeIcon from './components/volume-icon';
import Flashing from './components/flashing';

const timerLength = 10000;
const outerSize = 234; //size of tile itself
const innerSize = 200; //diameter of pomodoro

//not sure if this should be imported somehow or if copy and paste is the way to go
function daToDate(st) {
  var dub = function(n) {
    return parseInt(n) < 10 ? "0" + parseInt(n) : n.toString();
  };
  var da = st.split('..');
  var bigEnd = da[0].split('.');
  var lilEnd = da[1].split('.');
  var ds = `${bigEnd[0].slice(1)}-${dub(bigEnd[1])}-${dub(bigEnd[2])}T${dub(lilEnd[0])}:${dub(lilEnd[1])}:${dub(lilEnd[2])}Z`;
  return new Date(ds);
}

//polar to cartesian
var ptc = function(r, theta) {
  return {
    x: r * Math.cos(theta),
    y: r * Math.sin(theta)
  }
}

export default class TimerTile extends Component {

  constructor(props) {
    super(props);
    this.animate = this.animate.bind(this);
    this.state = this.getStateFromProps(props);
    this.state.playSound = true;
  }

  componentDidMount() {
    this.animate()
  }

  getStateFromProps(props) {
    if(props.data.charAt(0) == "~"){
      return {mode: "running", startTime: daToDate(props.data).getTime(), time: timerLength};
    }
    else if(props.data == "alarm"  ) {
      //api still delivers alarm events for cancelled timers, so make sure that it's actually time to fire the alarm before doing so
      if( this.state && this.state.mode == "running" && this.state.time < 500 ) { 
         return {mode: "alarm", time:0};
      }
      else { 
        if(!this.state) { return {mode: "waiting", time: timerLength} }
        else {
          //no change
          return {};
        }
      }
    }
    else {
      return {mode: "waiting", time: timerLength};
    }
  }

  componentWillReceiveProps(newProps) {    
    this.setState(this.getStateFromProps(newProps));
  }

  greenPart(ctx) {
    //green thing on the tomato that rotates
    const lightGreen = "#4da97a";
    const darkGreen = "#386f55";

    //define center of sprout based on time elapsed
    var ratio = (timerLength-this.state.time) / timerLength; //ratio of elapsed time to total time

    var easedRatio = ( -( Math.cos( Math.PI * ratio ) - 1 ) / 4) + (ratio/2);
    //from here https://easings.net/en#easeInOutSine
    

    //define scale based on time elapsed
    var easedRatioCubic  =  ratio < 0.5 ?
        4 * ratio * ratio * ratio :
        1 - Math.pow( -2 * ratio + 2, 3 ) / 2;

    var xCurve = outerSize/2-(.45*innerSize)*Math.sqrt(1-Math.pow((2*easedRatioCubic)-1,2));
    var center = {x: xCurve, y: .95*innerSize*easedRatioCubic +1.4*(outerSize-innerSize)/2,};

    var symmetricalEasedRatio =  easedRatioCubic > .5 ? 1 - ((easedRatioCubic+ratio)/2) : ((easedRatioCubic+ratio)/2);
    var scale = 1 + symmetricalEasedRatio*.6;


    var rotation = ((ratio+easedRatio)/2) * -1 * Math.PI;

    //generate star shape thing 
    var points = [];
    for(var i = 0; i < 14; i++) {
      var deg = i * (Math.PI / 7);
      var p = ptc( i % 2 == 0 ? 15  : 40, deg);

  
      points[i] = p;
    }

    ctx.save()
    ctx.translate(center.x, center.y)
    ctx.rotate(rotation);
    ctx.scale(scale, scale/2);

    ctx.fillStyle = lightGreen;
    ctx.beginPath();
    ctx.moveTo(points[0].x, points[0].y);
    for(var i = 1; i < points.length; i++) {
      ctx.lineTo(points[i].x,points[i].y);
    }
    ctx.closePath();
    ctx.fill();

    //draw the circle thing in the middle
    ctx.fillStyle = darkGreen;
    ctx.beginPath();

    function drawEllipse(ctx, x, y, w, h) {
      var kappa = .5522848,
          ox = (w / 2) * kappa, // control point offset horizontal
          oy = (h / 2) * kappa, // control point offset vertical
          xe = x + w,           // x-end
          ye = y + h,           // y-end
          xm = x + w / 2,       // x-middle
          ym = y + h / 2;       // y-middle
    
      ctx.beginPath();
      ctx.moveTo(x, ym);
      ctx.bezierCurveTo(x, ym - oy, xm - ox, y, xm, y);
      ctx.bezierCurveTo(xm + ox, y, xe, ym - oy, xe, ym);
      ctx.bezierCurveTo(xe, ym + oy, xm + ox, ye, xm, ye);
      ctx.bezierCurveTo(xm - ox, ye, x, ym + oy, x, ym);
      //ctx.closePath(); // not used correctly, see comments (use to close off open path)
      ctx.fill();
    }
    
    drawEllipse(ctx,-5,-5,10,10);

    ctx.restore()
  }

  animate() {
    //continuously animate
    var c = document.getElementById("timer-canvas");
    var ctx = c.getContext("2d");

    //clear
    ctx.clearRect(0, 0, c.width, c.height);

    //draw body of pomodoro clock
    ctx.fillStyle = "#ee5432";
    ctx.beginPath();
    ctx.arc(outerSize/2, outerSize/2, innerSize/2, 0, 2 * Math.PI);
    ctx.fill();
    
    //draw line separating top from bottom
    ctx.strokeStyle = "black";
    ctx.beginPath();
    ctx.moveTo((outerSize-innerSize)/2, outerSize/2);
    ctx.lineTo(innerSize+(outerSize-innerSize)/2,outerSize/2);
    ctx.stroke();

    this.greenPart(ctx);

    if(this.state.mode == "running") { 
      var time =  -1 * ((new Date()).getTime() - this.state.startTime);
      //javascript time can be ahead of the urbit alarm, so we dont want to show negative nubmers
      if(time < 0) { time = 0; }
      this.setState({time: time})
    }
    window.requestAnimationFrame(this.animate) 

  }

  formatTime(time) {
    var seconds = Math.ceil(time / 1000); //rounding up seems to result in better UX
    var minutes = Math.floor(seconds / 60);

    //convert to string
    seconds += "";
    minutes += "";

    if(seconds.length == 1) { seconds = "0" + seconds }
    if(minutes.length == 1) { minutes = "0" + minutes }

    return minutes + ":" + seconds;
  }

  startTimer() {
    api.action('timer', 'json', 'start');
  }

  stopTimer() {
    api.action('timer', 'json', 'stop');
  }

  renderWrapper(child) {
    return (
      <div style={{
        width: outerSize,
        height: outerSize,
        background: '#1a1a1a'
      }}>
        {child}
      </div>
    );
  }



  render() {

    var interaction;
    var interactionStyle = "link underline black hover-white";
    if(this.state.mode == "running") { 
      interaction = <a className={interactionStyle} onClick={this.stopTimer}>Stop</a>;
    }
    else if(this.state.mode == "alarm") {
      interaction = <a className={interactionStyle} onClick={this.stopTimer}>Shhh</a>;
    }
    else {
      interaction = <a className={interactionStyle} onClick={this.startTimer}>Start</a>;

    }
    
    return this.renderWrapper((
      <div style={{ position: "relative",
        fontFamily: "-apple-system,BlinkMacSystemFont,avenir next,avenir,helvetica neue,helvetica,ubuntu,roboto,noto,segoe ui,arial,sans-serif"
        }}>
        <canvas id="timer-canvas" width={outerSize} height={outerSize}></canvas>
        
        <div id="timer-display" style={{
          width: "100%",
          textAlign: "center",
          position: "absolute",
          top: "120px",
          left: "0px",
          fontSize:"28px",
          fontWeight:"300"
        }}>
         {this.state.mode == "running" ? this.formatTime(this.state.time) : 
          this.state.mode == "alarm" ?
          <div>
            <Flashing>00:00</Flashing>
            {
              this.state.playSound 
              ?
              <audio src="http://maxwellsfoley.com/ding.mp3" loop={true} autoPlay/>
              :
              null
            }
          </div>
          :
          '00:00'}
        </div>
        <div id="timer-interact" style={{
          width: "100%",
          textAlign: "center",
          position: "absolute",
          top: "170px",
          left: "0px",
          fontSize:"16px"
        }}>
          { interaction }
        </div>
        <div id="volume-container" style={{
          position: "absolute",
          top: "200px",
          left: "5px"
        }}>
          <VolumeIcon parent={this}/>
        </div>
      </div>
    ));
  }

}

window.timerTile = TimerTile;
