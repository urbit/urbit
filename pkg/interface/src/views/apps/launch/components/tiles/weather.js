import React from 'react';
import moment from 'moment';
import { Box, Icon, Text } from '@tlon/indigo-react';

import Tile from './tile';

export default class WeatherTile extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      latlong: '',
      manualEntry: false,
      error: false
    };
  }

  // geolocation and manual input functions
  locationSubmit() {
    navigator.geolocation.getCurrentPosition((res) => {
      const latlng = `${res.coords.latitude},${res.coords.longitude}`;
      this.setState({
        latlng
      }, (err) => {
        console.log(err);
      }, { maximumAge: Infinity, timeout: 10000 });
      this.props.api.weather(latlng);
      this.setState({ manualEntry: !this.state.manualEntry });
    });
  }

  manualLocationSubmit() {
    event.preventDefault();
    const gpsInput = document.getElementById('gps');
    const latlngNoSpace = gpsInput.value.replace(/\s+/g, '');
    const latlngParse = /-?[0-9]+(?:\.[0-9]*)?,-?[0-9]+(?:\.[0-9]*)?/g;
    if (latlngParse.test(latlngNoSpace)) {
      const latlng = latlngNoSpace;
      this.setState({ latlng }, (err) => {
      console.log(err);
      }, { maximumAge: Infinity, timeout: 10000 });
      this.props.api.launch.weather(latlng);
      this.setState({ manualEntry: !this.state.manualEntry });
    } else {
      this.setState({ error: true });
      return false;
    }
  }
  // set appearance based on weather
  setColors(data) {
    let weatherStyle = {
      bg: '',
      text: ''
    };

    switch (data.daily.icon) {
      case 'clear-day':
        weatherStyle = { bg: '#FEF4E0', text: 'black' };
        break;
      case 'clear-night':
        weatherStyle = { bg: '#000080', text: 'white' };
        break;
      case 'rain':
        weatherStyle = { bg: '#b0c7ff', text: 'black' };
        break;
      case 'snow':
        weatherStyle = { bg: '#f9f9f9', text: 'black' };
        break;
      case 'sleet':
        weatherStyle = { bg: '#f9f9f9', text: 'black' };
        break;
      case 'wind':
        weatherStyle = { bg: '#fff', text: 'black' };
        break;
      case 'fog':
        weatherStyle = { bg: '#fff', text: 'black' };
        break;
      case 'cloudy':
        weatherStyle = { bg: '#b1b2b3', text: 'black' };
        break;
      case 'partly-cloudy-day':
        weatherStyle = { bg: '#b1b2b3', text: 'black' };
        break;
      case 'partly-cloudy-night':
        weatherStyle = { bg: '#56668e', text: 'white' };
        break;
      default:
        weatherStyle = { bg: 'white', text: 'black' };
    }
    return weatherStyle;
  }
  // all tile views
  renderWrapper(child,
    weatherStyle = { bg: 'white', text: 'black' }
    ) {
    return (
      <Tile bg={weatherStyle.bg} color={weatherStyle.color}>
        {child}
      </Tile>
    );
  }

  renderManualEntry() {
    let secureCheck;
    let error;
    if (this.state.error === true) {
      error = <p className="f9 red2 pt1">Please try again.</p>;
    }
    if (location.protocol === 'https:') {
      secureCheck = (
        <a className="black white-d f9 pointer"
           onClick={() => this.locationSubmit()}>
          Detect ->
        </a>
      );
    }
    return this.renderWrapper(
      <Box
        display='flex'
        flexDirection='column'
        justifyContent='space-between'
        height='100%'
      >
        <a
          className="f9 black white-d pointer"
          onClick={() =>
            this.setState({ manualEntry: !this.state.manualEntry })
          }
        >
          &lt;&#45;
        </a>
        {secureCheck}
        <Text pb={1} mb='auto'>
          Please enter your{' '}
          <a
            className="bb"
            href="https://latitudeandlongitude.org/"
            target="_blank"
          >
            latitude and longitude
          </a>
          .
        </Text>
        {error}
        <form mt='auto' className="flex" style={{ marginBlockEnd: 0}}>
          <input
            id="gps"
            size="10"
            className="w-100 black white-d bg-transparent bn f9"
            type="text"
            placeholder="29.55, -95.08"
            onKeyDown={(e) => {
              if (e.key === 'Enter') {
                e.preventDefault();
                this.manualLocationSubmit(e.target.value);
              }
            }} />
          <input
            className={'bg-transparent black white-d bn pointer ' +
            'f9 flex-shrink-0 pr1'}
            type="submit"
            onClick={() => this.manualLocationSubmit()}
            value="->"
          />
        </form>
      </Box>
    );
  }

  renderNoData() {
    return this.renderWrapper(
      <Box
        bg='white'
        display='flex'
        flexDirection='column'
        justifyContent='space-between'
        height='100%'
        onClick={() => this.setState({ manualEntry: !this.state.manualEntry })}
      >
      <Box>
        <Icon icon='Weather' color='black' display='inline-block' verticalAlign='top' pt='3px' pr='2px' />
        <Text>Weather</Text>
      </Box>
        <Text style={{ cursor: 'pointer' }}>
          -> Set location
        </Text>
      </Box>
    );
  }

  renderWithData(data, weatherStyle) {
    const c = data.currently;
    const d = data.daily.data[0];

    const sunset = moment.unix(d.sunsetTime);
    const sunsetDiff = sunset.diff(moment(), 'hours');

    const sunrise = moment.unix(d.sunriseTime);
    let sunriseDiff = sunrise.diff(moment(), 'hours');

    if (sunriseDiff > 24) {
      sunriseDiff = sunriseDiff - 24;
    } else if (sunriseDiff < 0) {
      sunriseDiff = sunriseDiff + 24;
    }

    const nextSolarEvent = sunsetDiff > 0
      ? `Sun sets in ${sunsetDiff}h`
      : `Sun rises in ${sunriseDiff}h`;

    return this.renderWrapper(
      <Box
        width='100%'
        height='100%'
        display='flex'
        flexDirection='column'
        alignItems='space-between'
      >
        <Text>
          <Icon icon='Weather' color='black' display='inline' style={{ position: 'relative', top: '.3em'}} />
          Weather
          <a
          className='pointer'
            onClick={() =>
              this.setState({ manualEntry: !this.state.manualEntry })
            }
          >
          ->
        </a>
        </Text>

        <Box
          mt="auto"
          width="100%"
          display="flex"
          flexDirection="column"
        >
          <Text>{c.summary}</Text>
          <Text>{Math.round(c.temperature)}°</Text>
          <Text>{nextSolarEvent}</Text>
        </Box>
      </Box>
    , weatherStyle);
  }

  render() {
    const data = this.props.weather ? this.props.weather : {};

    if (this.state.manualEntry === true) {
      return this.renderManualEntry();
    }

    if ('currently' in data && 'daily' in data) {
      const weatherStyle = this.setColors(data);
      return this.renderWithData(data, weatherStyle);
    }

    if (this.props.location) {
      return this.renderWrapper((
        <div
          className={'pa2 w-100 h-100 ' +
          'bg-white bg-gray0-d black white-d'}>
            <Icon icon='Weather' color='black' display='inline' style={{ position: 'relative', top: '.3em' }} />
            <Text>Weather</Text>
          <p className="w-100 flex-col f9">
          Loading, please check again later...
          </p>
        </div>
      ));
    }
    return this.renderNoData();
  }
}

