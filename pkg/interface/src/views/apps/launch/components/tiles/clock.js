import React from 'react';
import moment from 'moment';
import SunCalc from 'suncalc';

import Tile from './tile';

const innerSize = 124; // clock size

// polar to cartesian
// var ptc = function(r, theta) {
//   return {
//     x: r * Math.cos(theta),
//     y: r * Math.sin(theta)
//   }
// }

let text = '#000000', background = '#ffffff';

const dark = window.matchMedia('(prefers-color-scheme: dark)');

if (dark.matches) {
  text = '#7f7f7f';
  background = '#333';
}

function darkColors(dark) {
  if (dark.matches) {
    text = '#7f7f7f';
    background = '#ffffff';
  }
 }

dark.addListener(darkColors);

const toRelativeTime = (date, referenceTime, unit) => moment(date)
  .diff(referenceTime, unit);

const minsToDegs = (mins) => {
  // 1440 = total minutes in an earth day
  return (mins / 1440) * 360;
};

const splitArc = (start, end) => end + ((start - end) * 0.5);

const isOdd = n => Math.abs(n % 2) == 1;

const radToDeg = rad => rad * (180 / Math.PI);

const degToRad = deg => deg * (Math.PI / 180);

const convert = (date, referenceTime) => {
  return minsToDegs(toRelativeTime(date, referenceTime, 'minutes'));
};

const circle = (ctx, x, y, r, from, to, fill) => {
  ctx.beginPath();
  ctx.arc( x, y, r, from, to );
  ctx.strokeStyle = 'rgba(0,0,0,0)';
  ctx.fillStyle = fill || 'rgba(0,0,0,0)';
  ctx.fill();
};

const circleClip = (ctx, x, y, r, from, to, fill) => {
  ctx.globalCompositeOperation = 'xor';
  circle(ctx, x, y, r, from, to, fill);
  ctx.globalCompositeOperation = 'source-over';
};

const circleOutline = (ctx, x, y, r, from, to, stroke, lineWidth) => {
  ctx.beginPath();
  ctx.arc( x, y, r, from, to );
  ctx.fillStyle = 'rgba(0,0,0,0)';
  ctx.lineWidth = lineWidth;
  ctx.strokeStyle = stroke || 'rgba(0,0,0,0)';
  if (lineWidth) {
    ctx.stroke();  
  }
};

const arc = (ctx, x, y, r, from, to, fill) => {
  ctx.beginPath();
  ctx.arc( x, y, r, from, to );
  ctx.fillStyle = 'rgba(0,0,0,0)';
  ctx.lineWidth = r * 2;
  ctx.strokeStyle = fill || 'rgba(0,0,0,0)';
  ctx.stroke();
};

const degArc = (ctx, x, y, r, from, to, fill) => {
  ctx.beginPath();
  ctx.arc( x, y, r, degToRad(from), degToRad(to));
  ctx.fillStyle = 'rgba(0,0,0,0)';
  ctx.lineWidth = r * 2;
  ctx.strokeStyle = fill || 'rgba(0,0,0,0)';
  ctx.stroke();
};

class Clock extends React.Component {
  constructor(props) {
    super(props);
    this.animate = this.animate.bind(this);
    this.canvasRef = React.createRef();
    this.canvas = null;
    this.angle = 0;
    this.referenceTime = moment().startOf('day').subtract(6, 'hours');
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
      nauticalDusk: 0
      // sunriseStartTime = 1509967519,
      // sunriseEndTime = 2500 + 1509967519,
      // sunsetStartTime = 1510003982,
      // sunsetEndTime
      // moonPhase = 0.59,
    };
  }

  initGeolocation() {
    if (typeof this.props.data === 'string') {
      const latlon = this.props.data.split(',');
      const lat = latlon[0];
      const lon = latlon[1];

      const suncalc = SunCalc.getTimes(new Date(), lat, lon);

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
        nauticalDusk: convert(suncalc.nauticalDusk, this.referenceTime)
      };

      this.setState({
        lat,
        lon,
        ...convertedSunCalc,
        geolocationSuccess: true
      });
    }
  }

  componentDidUpdate(prevProps) {
    if (prevProps !== this.props) {
      this.initGeolocation();
    }
  }

  componentDidMount() {
    this.canvas = initCanvas(
      this.canvasRef,
      { x: innerSize, y: innerSize },
      4
    );

    this.initGeolocation();
    this.animate();
  }

  componentWillUnmount() {
    if (this.animationTimer) {
      window.clearTimeout(this.animationTimer);
    }
  }

  animate() {
    this.animationTimer = 
      window.setTimeout(() => window.requestAnimationFrame(this.animate), 1000);

    const { state } = this;
    const time = new Date();
    const ctx = this.canvas.getContext('2d');
    ctx.clearRect(0, 0, ctx.width, ctx.height);
    ctx.save();

    const ctr = innerSize / 2;

    // Sun+moon calculations
    const cx = ctr;
    const cy = ctr;
    this.angle = degToRad(convert(time, this.referenceTime));
    const newX = cx + (ctr - 15) * Math.cos(this.angle);
    const newY = cy + (ctr - 15) * Math.sin(this.angle);

    // Center white circle with time and date
    circle(
      ctx,
      ctr,
      ctr,
      ctr,
      -1,
      2 * Math.PI,
      background
    );

    // Day
    degArc(
      ctx,
      ctr,
      ctr,
      ctr / 2,
      state.sunriseEnd,
      state.sunset,
      'rgba(33, 157, 255, .3)'
    );

    // Sunrise
    degArc(
      ctx,
      ctr,
      ctr,
      ctr / 2,
      state.sunsetStart,
      state.sunriseEnd,
      '#FFC700'
    );

    // Sunset
    degArc(
      ctx,
      ctr,
      ctr,
      ctr / 2,
      state.dusk,
      state.dawn,
      'rgba(255, 65, 54, .8)'
    );

    // Night
    degArc(
      ctx,
      ctr,
      ctr,
      ctr / 2,
      state.night,
      state.nightEnd,
      'rgba(0, 0, 0, .8)'
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
      );

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
      );
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


    // Outer borders
    circleOutline(
      ctx,
      ctr,
      ctr,
      ctr-1,
      -1,
      2 * Math.PI,
      'none',
      0
    );

    // Center white circle border
    circleOutline(
      ctx,
      ctr,
      ctr,
      ctr/1.85,
      -1,
      2 * Math.PI,
      'none',
      0
    );

    // Inner hole
    circle(
      ctx,
      ctr,
      ctr,
      ctr/1.85,
      -1,
      2 * Math.PI,
      background
    );

    // Text for time and date
    const timeText = isOdd(time.getSeconds())
      ? moment().format('h mm A')
      : moment().format('h:mm A');
    const dateText = moment().format('MMM Do');
    ctx.textAlign = 'center';
    ctx.fillStyle = text;
    ctx.font = '12px Inter';
    ctx.fillText(timeText, ctr, ctr + 6 - 7);
    ctx.fillStyle = text;
    ctx.font = '12px Inter';
    ctx.fillText(dateText, ctr, ctr + 6 + 7);

    ctx.restore();
  }

  render() {
    return (
      <canvas
        style={{ height: '100%', width: '100%'}}
        ref={ canvasRef => this.canvasRef = canvasRef }
        id="clock-canvas"
      />
    );
  }
}

export default class ClockTile extends React.Component {
  constructor(props) {
    super(props);
  }

  renderWrapper(child) {
    return (
      <Tile p={0} border={0}>
        {child}
      </Tile>
    );
  }

  render() {
    const data = this.props.location ? this.props.location : {};
    return this.renderWrapper((
      <Clock data={data} />
    ));
  }
}

const initCanvas = (canvas, size, ratio) => {
  const { x, y } = size;
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
};

