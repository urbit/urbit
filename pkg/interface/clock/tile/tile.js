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
    this.canvasRef = React.createRef();
    this.dodecagonImg = null;
    this.canvas = null;

  }

  componentDidMount() {


    this.canvas = initCanvas(
      this.canvasRef,
      { x: innerSize, y: innerSize },
      4
    );

    this.animate()
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
    var c = this.canvas
    var ctx = c.getContext("2d");
    ctx.clearRect(0, 0, c.width, c.height);
    ctx.save();
    ctx.translate(0.5, 0.5); //forces antialias thus smoothing out jagged lines

    //draw the clock itself
    // this.dodecagon(ctx);

    //draw the hands
    this.secondHand(ctx, time);

    this.hourHand(ctx, time);
    this.minuteHand(ctx, time);

    ctx.restore();

    window.requestAnimationFrame(this.animate)
  }

  render() {
    console.log('hi')
    return <div style={{position:'relative'}}>

        <svg style={{position:'absolute'}} width="218" height="218" viewBox="0 0 234 234" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path d="M112.859 1.10961C115.572 0.38269 118.428 0.38269 121.141 1.10961L171.359 14.5654C174.072 15.2923 176.546 16.7206 178.531 18.7065L215.293 55.4685C217.279 57.4545 218.708 59.9282 219.435 62.6411L232.89 112.859C233.617 115.572 233.617 118.428 232.89 121.141L219.435 171.359C218.708 174.072 217.279 176.546 215.293 178.531L178.531 215.293C176.546 217.279 174.072 218.708 171.359 219.435L121.141 232.89C118.428 233.617 115.572 233.617 112.859 232.89L62.6411 219.435C59.9282 218.708 57.4545 217.279 55.4685 215.293L18.7065 178.531C16.7206 176.546 15.2923 174.072 14.5654 171.359L1.10961 121.141C0.38269 118.428 0.38269 115.572 1.10961 112.859L14.5654 62.6411C15.2923 59.9282 16.7206 57.4545 18.7065 55.4685L55.4685 18.7065C57.4545 16.7206 59.9282 15.2923 62.6411 14.5654L112.859 1.10961Z" fill="white"/>
        </svg>

        <canvas
        style={{position:'absolute'}}
        ref={ canvasRef => this.canvasRef = canvasRef }
        id="clock-canvas"/>
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
        background: 'rgba(0,0,0,1)'
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

const loadImg = (base64, cb) => new Promise(resolve => {
  const img = new Image();
  img.onload = () => resolve(cb(img));
  img.onerror = () => reject('Error loading image');
  img.src = base64;
});


const initCanvas = (canvas, size, ratio) => {
  const { x, y } = size;
  let ctx = canvas.getContext('2d');

  // let ratio = ctx.webkitBackingStorePixelRatio < 2
  //   ? window.devicePixelRatio
  //   : 1;

  // default for high print resolution.
  // ratio = ratio * resMult;


  canvas.width = x * ratio;
  canvas.height = y * ratio;
  canvas.style.width = x + 'px';
  canvas.style.height = y + 'px';

  canvas.getContext('2d').scale(ratio, ratio);

  return canvas;
}

window.clockTile = ClockTile;
