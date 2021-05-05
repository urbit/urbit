/* eslint-disable max-lines-per-function */
import moment from 'moment';
import React from 'react';
import styled from 'styled-components';
import SunCalc from 'suncalc';
import Tile from './tile';

const VIEWBOX_SIZE = 100;
const CX = VIEWBOX_SIZE / 2;
const CY = VIEWBOX_SIZE / 2;
const RADIUS = VIEWBOX_SIZE / 2;
const CELESTIAL_BODY_SIZE = 16;

const ApplyClockBg = styled.div`
  .background {
    fill: ${p => p.theme.colors.white};
  }
  .time {
    fill: ${p => p.theme.colors.black};
    color: ${p => p.theme.colors.black};
  }
  .date {
    fill: ${p => p.theme.colors.gray};
  }
`;

const toRelativeTime = (date, referenceTime, unit) => moment(date)
  .diff(referenceTime, unit);

const minsToDegs = (mins) => {
  // 1440 = total minutes in an earth day
  return (mins / 1440) * 360;
};

const degToRad = deg => deg * (Math.PI / 180);

const convert = (date, referenceTime) => {
  return minsToDegs(toRelativeTime(date, referenceTime, 'minutes'));
};

// https://github.com/tingletech/moon-phase
export const dFromPhase = (moonPhase) => {
  let mag, sweep, d = 'm50,0';
  if (moonPhase <= 0.25) {
    sweep = [1, 0];
    mag = 20 - 20 * moonPhase * 4;
  } else if (moonPhase <= 0.50) {
    sweep = [0, 0];
    mag = 20 * (moonPhase - 0.25) * 4;
  } else if (moonPhase <= 0.75) {
    sweep = [1, 1];
    mag = 20 - 20 * (moonPhase - 0.50) * 4;
  } else if (moonPhase <= 1) {
    sweep = [0, 1];
    mag = 20 * (moonPhase - 0.75) * 4;
  }

  d = d + 'a' + mag + ',20 0 1,' + sweep[0] + ' 0,100 ';
  d = d + 'a20,20 0 1,' + sweep[1] + ' 0,-100';
  return d;
};

const Moon = ({ angle, ...props }) => {
  const phase = SunCalc.getMoonIllumination(moment().toDate()).phase.toFixed(2);
  const cx = CX + (RADIUS - 12) * Math.cos(degToRad(angle)) - (CELESTIAL_BODY_SIZE / 2);
  const cy = CY + (RADIUS - 12) * Math.sin(degToRad(angle)) - (CELESTIAL_BODY_SIZE / 2);
  return (
    <g>
      <mask id="umbra">
        <rect x="-50" y="-50" height="200" width="200" fill="black" />
        <path d={dFromPhase(phase)} fill="white" />
      </mask>
      <circle
        id="moonbg"
        cx={CX + (RADIUS - 12) * Math.cos(degToRad(angle))}
        cy={CY + (RADIUS - 12) * Math.sin(degToRad(angle))}
        fill="#ADA09D"
        r={CELESTIAL_BODY_SIZE / 2}
        {...props}
      ></circle>
      <use
      width={CELESTIAL_BODY_SIZE}
      height={CELESTIAL_BODY_SIZE}
      xlinkHref="#Moon-symbol"
      id="moon"
      x={cx}
      y={cy}
      transform={`rotate(${angle} ${cx + (CELESTIAL_BODY_SIZE / 2)} ${cy + (CELESTIAL_BODY_SIZE / 2)})`}
      />
    </g>
  );
};

const Sun = ({ angle, ...props }) => (
  <circle
    id="sun"
    cx={CX + (RADIUS - 12) * Math.cos(degToRad(angle))}
    cy={CY + (RADIUS - 12) * Math.sin(degToRad(angle))}
    fill="#FCC440"
    stroke="rgba(0,0,0,0.1)"
    r={CELESTIAL_BODY_SIZE / 2}
    {...props}
  ></circle>
);

const SvgArc = ({ start, end, ...rest }) => {
  const x1 = CX + RADIUS * Math.cos(degToRad(start));
  const y1 = CY + RADIUS * Math.sin(degToRad(start));
  const x2 = CX + RADIUS * Math.cos(degToRad(end));
  const y2 = CY + RADIUS * Math.sin(degToRad(end));

  // const isLarge = Math.abs((start > 360 ? start - 360 : start) - end) > 180;

  const d = [
    'M', CX, CY,
    'L', x1, y1,
    'A', RADIUS, RADIUS, '0', '1', '1', x2, y2, 'z'
  ].join(' ');

  return <path d={d} {...rest} />;
};

class ClockText extends React.Component<ClockTextProps, ClockTextState> {
  interval?: NodeJS.Timeout;
  constructor(props) {
    super(props);
    this.state = {
      time: Date.now()
    };
  }

  componentDidMount() {
    this.interval = setInterval(() => this.setState({ time: Date.now() }), 1000);
  }

  componentWillUnmount() {
    if (this.interval) {
      clearInterval(this.interval);
    }
  }

  render() {
    const now = moment(this.state.time);
    return (
      <>
        <use xlinkHref="#clock-center" className="background" />
        <text
          textAnchor="middle"
          x={CX}
          y={CY - 2}
          fontSize="10"
          fontFamily="Inter"
          className="time"
        >
          <tspan>{now.format('h')}</tspan>
          <tspan>:<animate attributeName="fill"
            values="currentColor;transparent"
            begin="0s"
            dur="1s"
            calcMode="discrete"
            repeatCount="indefinite"
                  />
          </tspan>
          <tspan>{now.format('mm A')}</tspan>
        </text>
        <text
          textAnchor="middle"
          x={CX}
          y={CY + 11}
          fontSize="10"
          fontFamily="Inter"
          className="date"
        >{now.format('MMM D')}<tspan style={{ fontFeatureSettings: '\'sups\' 1' }}>{now.format('Do').replace(now.format('D'), '')}</tspan></text>
      </>
    );
  }
}

class Clock extends React.PureComponent {
  constructor(props) {
    super(props);
    this.angle = 0;
    this.referenceTime = moment().startOf('day').subtract(6, 'hours');
    this.state = {
      time: Date.now(),
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
    if (typeof this.props.data === 'object') {
      const { latitude: lat, longitude: lon } = this.props.data;

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
    this.initGeolocation();
    this.interval = setInterval(() => this.setState({ time: Date.now() }), 60000);
  }

  componentWillUnmount() {
    clearInterval(this.interval);
  }

  render() {
    const now = moment(this.state.time);
    const angle = convert(now, this.referenceTime);

    return (
      <ApplyClockBg>
        <svg
          style={{ height: '100%', width: '100%' }}
          viewBox={`0 0 ${VIEWBOX_SIZE} ${VIEWBOX_SIZE}`}
          xmlns="http://www.w3.org/2000/svg"
          xmlnsXlink="http://www.w3.org/1999/xlink"
        >
          <defs>
            <symbol id="border">
              <circle cx={CY} cy={CY} r={RADIUS} />
            </symbol>
            <symbol id="clock-center">
              <circle r={VIEWBOX_SIZE / 1.85 / 2} cx={CX} cy={CY} />
            </symbol>
            <mask id="center-mask">
              <use xlinkHref="#border" fill="white" />
              <use xlinkHref="#clock-center" fill="black" />
            </mask>
            <symbol id="Moon-symbol" viewBox="0 0 100 100" preserveAspectRatio="xMidYMid">
              <g>
                <path mask="url(#umbra)" d="m50,0 a20,20 0 1,1 0,100 a20,20 0 1,1 0,-100" fill="#fff" stroke="#000" />
              </g>
            </symbol>

          </defs>
          <g mask="url(#center-mask)">
            <use xlinkHref="#border" className="background" />
            <SvgArc
              id="day"
              start={this.state.sunriseEnd}
              end={this.state.sunset}
              fill="rgba(33, 157, 255, .2)"
            />
            <SvgArc
              id="sunrise"
              start={this.state.sunsetStart}
              end={this.state.sunriseEnd}
              fill="#FFC700"
            />
            <SvgArc
              id="sunset"
              start={this.state.dusk}
              end={this.state.dawn}
              fill="rgba(255, 65, 54, .8)"
            />
            <SvgArc
              id="night"
              start={this.state.night}
              end={this.state.nightEnd}
              fill="rgba(0, 0, 0, .8)"
            />
            {angle > this.state.nightEnd && angle < this.state.sunset
              ? <Sun angle={angle} />
              : <Moon angle={angle} />
            }
          </g>
          <ClockText />
        </svg>
      </ApplyClockBg>
    );
  }
}

const ClockTile = ({ location = {} }) => (
  <Tile p={0} border={0} bg='transparent' boxShadow='none'>
    <Clock data={location} />
  </Tile>
);

export default ClockTile;
