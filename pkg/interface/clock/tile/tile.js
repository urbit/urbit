import React, { Component } from 'react';
import classnames from 'classnames';
import moment from 'moment'
import SunCalc from 'suncalc'

const outerSize = 124; //tile size
const innerSize = 124; //clock size

//polar to cartesian
// var ptc = function(r, theta) {
//   return {
//     x: r * Math.cos(theta),
//     y: r * Math.sin(theta)
//   }
// }

let text = "#000000", background = "#ffffff";

let dark = window.matchMedia('(prefers-color-scheme: dark)');

if (dark.matches) {
  text = "#7f7f7f";
  background = "#333";
}

function darkColors(dark) {
  if (dark.matches) {
    text = "#7f7f7f";
    background = "#333";
  } else {
    text = "#000000";
    background = "#ffffff"
  }
 }

dark.addListener(darkColors);


const toRelativeTime = (date, referenceTime, unit) => moment(date)
  .diff(referenceTime, unit)

const minsToDegs = (mins) => {
  // 1440 = total minutes in an earth day
  return (mins / 1440) * 360
}

const clockwise = (deg, delta) => deg + delta

const anticlockwise = (deg, delta) => deg - delta

const splitArc = (start, end) => end + ((start - end) * 0.5)

const isOdd = n => Math.abs(n % 2) == 1;

const radToDeg = (rad) => rad * (180 / Math.PI);

const degToRad = (deg) => deg * (Math.PI / 180);

const convert = (date, referenceTime) => {
  return minsToDegs(toRelativeTime(date, referenceTime, 'minutes'))
}

const circle = (ctx, x, y, r, from, to, fill) => {
  ctx.beginPath();
  ctx.arc( x, y, r, from, to, );
  ctx.strokeStyle = 'rgba(0,0,0,0)';
  ctx.fillStyle = fill || 'rgba(0,0,0,0)';
  ctx.fill();
}

const circleOutline = (ctx, x, y, r, from, to, stroke, lineWidth) => {
  ctx.beginPath();
  ctx.arc( x, y, r, from, to, );
  ctx.fillStyle = 'rgba(0,0,0,0)';
  ctx.lineWidth = lineWidth;
  ctx.strokeStyle = stroke || 'rgba(0,0,0,0)';
  ctx.stroke();
}

const arc = (ctx, x, y, r, from, to, fill) => {
  ctx.beginPath();
  ctx.arc( x, y, r, from, to, );
  ctx.fillStyle = 'rgba(0,0,0,0)';
  ctx.lineWidth = r * 2;
  ctx.strokeStyle = fill || 'rgba(0,0,0,0)';
  ctx.stroke();
}

const degArc = (ctx, x, y, r, from, to, fill) => {
  ctx.beginPath();
  ctx.arc( x, y, r, degToRad(from), degToRad(to));
  ctx.fillStyle = 'rgba(0,0,0,0)';
  ctx.lineWidth = r * 2;
  ctx.strokeStyle = fill || 'rgba(0,0,0,0)';
  ctx.stroke();
}

class Clock extends Component {

  constructor(props) {
    super(props);
    this.animate = this.animate.bind(this);
    this.canvasRef = React.createRef();
    this.canvas = null;
    this.angle = 0;
    this.referenceTime = moment().startOf('day').subtract(6, 'hours')
    this.state = {
      lat: 0,
      lon: 0,
      geolocationSuccess: false,
      sunrise: 0,
      sunsetStart: 0,
      sunset:0,
      sunriseEnd: 0,
      dusk: 0,
      dawn: 0,
      night: 0,
      nightEnd: 0,
      nauticalDawn: 0,
      nauticalDusk: 0,
      // sunriseStartTime = 1509967519,
      // sunriseEndTime = 2500 + 1509967519,
      // sunsetStartTime = 1510003982,
      // sunsetEndTime
      // moonPhase = 0.59,
    }

  }

  initGeolocation() {
    if (typeof this.props.data === 'string') {
      // console.log(typeof this.props.data)
      const latlon = this.props.data.split(',')
      const lat = latlon[0]
      const lon = latlon[1]

      const suncalc = SunCalc.getTimes(new Date(), lat, lon)

      const convertedSunCalc = {
        sunset: convert(suncalc.sunset, this.referenceTime),
        sunrise: convert(suncalc.sunrise, this.referenceTime),
        sunsetStart: convert(suncalc.sunsetStart, this.referenceTime),
        sunriseEnd: convert(suncalc.sunriseEnd, this.referenceTime),
        dusk: convert(suncalc.dusk, this.referenceTime),
        dawn: convert(suncalc.dawn, this.referenceTime),
        night: convert(suncalc.night, this.referenceTime),
        nightEnd: convert(suncalc.nightEnd, this.referenceTime),
        nauticalDawn: convert(suncalc.nauticalDawn, this.referenceTime),
        nauticalDusk: convert(suncalc.nauticalDusk, this.referenceTime),
      }

      this.setState({
        lat,
        lon,
        ...convertedSunCalc,
        geolocationSuccess: true,
      })
    }
  }

  componentDidUpdate(prevProps) {
    if (prevProps !== this.props) {
      this.initGeolocation()
    }
  }


  componentDidMount() {
    this.canvas = initCanvas(
      this.canvasRef,
      { x: innerSize, y: innerSize },
      4
    );

    this.initGeolocation()
    this.animate()
  }


  animate() {
    window.setTimeout(() => window.requestAnimationFrame(this.animate), 1000)

    const { state } = this
    const time = new Date();
    const ctx = this.canvas.getContext("2d");
    ctx.clearRect(0, 0, ctx.width, ctx.height);
    ctx.save();

    const ctr = innerSize / 2

    // Sun+moon calculations
    const dd = 4
    var cx = ctr
    var cy = ctr
    this.angle = degToRad(convert(time, this.referenceTime))
    var newX = cx + (ctr - 15) * Math.cos(this.angle);
    var newY = cy + (ctr - 15) * Math.sin(this.angle);

    // Initial background
    circle(
      ctx,
      ctr,
      ctr,
      ctr,
      -1,
      2 * Math.PI,
      background
    )

    // Day
    degArc(
      ctx,
      ctr,
      ctr,
      ctr / 2,
      state.sunriseEnd,
      state.sunset,
      '#6792FF'
    );

    // Sunrise
    degArc(
      ctx,
      ctr,
      ctr,
      ctr / 2,
      state.nightEnd,
      state.sunriseEnd,
      '#FCC440'
    );

    // Sunset
    degArc(
      ctx,
      ctr,
      ctr,
      ctr / 2,
      state.nightEnd,
      splitArc(state.sunriseEnd, state.nightEnd),
      '#FF611E'
    );

    // Sunset
    degArc(
      ctx,
      ctr,
      ctr,
      ctr / 2,
      state.sunset,
      state.night,
      '#FCC440'
    );

    // Sunset
    degArc(
      ctx,
      ctr,
      ctr,
      ctr / 2,
      splitArc(state.sunset, state.night),
      state.night,
      '#FF611E'
    );

    if (
      radToDeg(this.angle) > splitArc(state.sunriseEnd, state.nightEnd)
      && radToDeg(this.angle) < splitArc(state.sunset, state.night)
    ) {
      // Sun circle
      circle(
        ctx,
        newX-1/2,
        newY-1/2,
        8,
        0,
        2 * Math.PI,
        '#FCC440'
      )

      // Sun circle border
      circleOutline(
        ctx,
        newX-1/2,
        newY-1/2,
        8,
        0,
        2 * Math.PI,
        '#6792FF',
        1
      );
    } else {
      // Moon circle
      circle(
        ctx,
        newX-1/2,
        newY-1/2,
        8,
        0,
        2 * Math.PI,
        '#FFFFFF'
      )
      // Moon circle outline
      circleOutline(
        ctx,
        newX-1/2,
        newY-1/2,
        8,
        0,
        2 * Math.PI,
        '#000000',
        1
      );
    }

    // Night
    degArc(
      ctx,
      ctr,
      ctr,
      ctr / 2,
      state.night,
      state.nightEnd,
      'rgb(26, 26, 26)'
    );

    // Outer borders
    circleOutline(
      ctx,
      ctr,
      ctr,
      ctr-1,
      -1,
      2 * Math.PI,
      text,
      1
    );

    // Outer borders
    circleOutline(
      ctx,
      ctr,
      ctr,
      ctr,
      -1,
      2 * Math.PI,
      background,
      1
    );

    // Center white circle with time and date
    circle(
      ctx,
      ctr,
      ctr,
      ctr/1.85,
      -1,
      2 * Math.PI,
      background
    )

    // Center white circle border
    circleOutline(
      ctx,
      ctr,
      ctr,
      ctr/1.85,
      -1,
      2 * Math.PI,
      text,
      1
    );

    // Text for time and date
    const timeText = isOdd(time.getSeconds())
      ? moment().format('h mm A')
      : moment().format('h:mm A')
    const dateText = moment().format('MMM Do')
    ctx.textAlign = 'center'
    ctx.fillStyle = text
    ctx.font = '12px Inter'
    ctx.fillText(timeText, ctr, ctr + 6 - 7)
    ctx.fillStyle = text
    ctx.font = '12px Inter'
    ctx.fillText(dateText, ctr, ctr + 6 + 7)

    ctx.restore();
  }

  render() {
    return <div style={{position:'relative'}}>
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
      <div className="bg-white bg-gray0-d" style={{
        width: outerSize,
        height: outerSize,
      }}>
        {child}
      </div>
    );
  }

  render() {
    let data = !!this.props.data ? this.props.data : {};
    return this.renderWrapper((
      <Clock data={data}/>
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
