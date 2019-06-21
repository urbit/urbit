import React, { Component } from 'react';
import classnames from 'classnames';

const outerSize = 234; //tile size
const innerSize = 218; //clock size

//polar to cartesian
var ptc = function(r, theta) {
  return {
    x: r * Math.cos(theta),
    y: r * Math.sin(theta)
  }
}

class Clock extends Component {

  constructor(props) {
    super(props);
    this.animate = this.animate.bind(this);
    this.hourHand = this.hourHand.bind(this);
    this.minuteHand = this.minuteHand.bind(this);
    this.secondHand = this.secondHand.bind(this);

  }

  componentDidMount() {
    this.animate()
  }

  dodecagon(ctx) {
    var points = [];
    //first create array of twelve points
    for(var i = 0; i < 12; i++) {
      var deg = i * (Math.PI / 6);
      var p = ptc(innerSize/2, deg);
      p.x += innerSize/2;
      p.y += innerSize/2;  
      points[i] = p;
    }
    ctx.fillStyle = "white";
    ctx.beginPath();
    ctx.moveTo(points[0].x, points[0].y);
    for(var i = 1; i < points.length; i++) {
      ctx.lineTo(points[i].x,points[i].y);
    }
    ctx.closePath();
    ctx.fill();
  }

  hourHand(ctx, time) {
    //get number of minutes in the day so far, this is so hour hand moves gradually
    //  rather than in large ticks
    var mins =  time.getMinutes() + (60 * time.getHours());

    //draw the circle thing in the middle
    ctx.fillStyle = "black";
    ctx.beginPath();
    ctx.arc(innerSize/2, innerSize/2, 5, 0, 2 * Math.PI);
    ctx.fill();

    //draw the actual hour hand
    ctx.strokeStyle = "black";
    ctx.beginPath();
    ctx.moveTo(innerSize/2, innerSize/2);
    var angle = (Math.PI/-2.0) + ((2*Math.PI/(12*60)) * mins);   
    var p = ptc(innerSize*.22, angle);
    p.x += innerSize/2; 
    p.y += innerSize/2;
    ctx.lineTo(p.x,p.y);
    ctx.stroke();
  }

  minuteHand(ctx, time) {
    //number of seconds in the hour so far
    var secs =  time.getSeconds() + (60 * time.getMinutes());
    ctx.strokeStyle = "black";
    ctx.beginPath();
    ctx.moveTo(innerSize/2, innerSize/2);
    var angle = (Math.PI/-2.0) + ((2*Math.PI/(60*60)) * secs);   
    var p = ptc(innerSize*.35, angle);
    p.x += innerSize/2; 
    p.y += innerSize/2;
    ctx.lineTo(p.x,p.y);
    ctx.stroke();

  }

  secondHand(ctx, time) {
    //get number of ms in minute so far
    var secs =  time.getSeconds();

    let middle = {x: innerSize/2, y: innerSize*.75};
    //draw the circle thing in the middle
    ctx.fillStyle = "red";
    ctx.beginPath();
    ctx.arc(middle.x, middle.y, 5, 0, 2 * Math.PI);
    ctx.fill();

    //draw the actual second hand
    ctx.strokeStyle = "red";
    ctx.beginPath();
    var angle = (Math.PI/-2.0) + ((2*Math.PI/(60)) * secs);   
    var p = ptc(30, angle);
    var p2 = ptc(-10, angle ); //starting point is a little bit off center in the opposite direction
    p.x += middle.x; 
    p.y += middle.y;
    p2.x += middle.x;
    p2.y += middle.y
    ctx.moveTo(p2.x, p2.y);
    ctx.lineTo(p.x,p.y);
    ctx.stroke();
  }

  animate() {
    var time = new Date();
    //continuously animate
    var c = document.getElementById("clock-canvas");
    var ctx = c.getContext("2d");
    ctx.clearRect(0, 0, c.width, c.height);
    ctx.save();
    ctx.translate(0.5, 0.5); //forces antialias thus smoothing out jagged lines

    //draw the clock itself
    this.dodecagon(ctx);

    //draw the hands
    this.secondHand(ctx, time);

    this.hourHand(ctx, time);
    this.minuteHand(ctx, time);

    ctx.restore();

    window.requestAnimationFrame(this.animate)
  }

  render() {
    return <div>
      <canvas id="clock-canvas" width={innerSize} height={innerSize}></canvas>
    </div>
  }
}

export default class ClockTile extends Component {

  constructor(props) {
    super(props);
  }

  renderWrapper(child) {
    return (
      <div className="pa2" style={{
        width: outerSize,
        height: outerSize,
        background: '#1a1a1a'
      }}>
        {child}
      </div>
    );
  }

  render() {
    let data = !!this.props.data ? this.props.data : {};

    return this.renderWrapper((
      <Clock/>
    ));

  }

}

window.clockTile = ClockTile;
