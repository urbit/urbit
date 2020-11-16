import React from 'react';
import moment from 'moment';
import { Box, Icon, Text, BaseAnchor, BaseInput } from '@tlon/indigo-react';

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
      this.props.api.launch.weather(latlng);
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

    switch (data.currently.icon) {
      case 'clear-day':
        weatherStyle = { bg: '#E9F5FF', text: '#333' };
        break;
      case 'clear-night':
        weatherStyle = { bg: '#14263C', text: '#fff' };
        break;
      case 'rain':
        weatherStyle = { bg: '#2E1611', text: '#fff' };
        break;
      case 'snow':
        weatherStyle = { bg: '#F9F9FB', text: '#333' };
        break;
      case 'sleet':
        weatherStyle = { bg: '#EFF1F3', text: '#333' };
        break;
      case 'wind':
        weatherStyle = { bg: '#F7FEF6', text: '#333' };
        break;
      case 'fog':
        weatherStyle = { bg: '#504D44', text: '#fff' };
        break;
      case 'cloudy':
        weatherStyle = { bg: '#EEF1F5', text: '#333' };
        break;
      case 'partly-cloudy-day':
        weatherStyle = { bg: '#F3F6FA', text: '#333' };
        break;
      case 'partly-cloudy-night':
        weatherStyle = { bg: '#283442', text: '#fff' };
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
      <Tile bg={weatherStyle.bg}>
        {child}
      </Tile>
    );
  }

  renderManualEntry() {
    let secureCheck;
    let error;
    if (this.state.error === true) {
      error = <Text display='block' color='red' pt='1'>Please try again.</Text>;
    }
    if (location.protocol === 'https:') {
      secureCheck = (
        <Text color='black' cursor='pointer'
           onClick={() => this.locationSubmit()}
        >
          Detect ->
        </Text>
      );
    }
    return this.renderWrapper(
      <Box
        display='flex'
        flexDirection='column'
        justifyContent='space-between'
        height='100%'
      >
        <Text
          color='black'
          cursor='pointer'
          onClick={() =>
            this.setState({ manualEntry: !this.state.manualEntry })
          }
        >
          &lt;&#45;
        </Text>
        {secureCheck}
        <Text pb={1} mb='auto'>
          Please enter your{' '}
          <BaseAnchor
            borderBottom='1px solid'
            color='black'
            href="https://latitudeandlongitude.org/"
            target="_blank"
          >
            latitude and longitude
          </BaseAnchor>
          .
        </Text>
        {error}
        <Box mt='auto' display='flex' marginBlockEnd='0'>
          <BaseInput
            id="gps"
            size="10"
            width='100%'
            color='black'
            fontSize='0'
            backgroundColor='transparent'
            border='0'
            type="text"
            placeholder="29.55, -95.08"
            onKeyDown={(e) => {
              if (e.key === 'Enter') {
                e.preventDefault();
                this.manualLocationSubmit(e.target.value);
              }
            }}
          />
          <BaseInput
            backgroundColor='transparent'
            color='black'
            cursor='pointer'
            pl='1'
            fontSize='0'
            border='0'
            type="submit"
            onClick={() => this.manualLocationSubmit()}
            value="->"
          />
        </Box>
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
          <Icon icon='Weather' display='inline-block' verticalAlign='top' pt='3px' pr='2px' />
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
        <Text color={weatherStyle.text}>
          <Icon icon='Weather' color={weatherStyle.text} display='inline' style={{ position: 'relative', top: '.3em' }} />
          Weather
            <Text
              color={weatherStyle.text}
              cursor='pointer'
              onClick={() =>
                this.setState({ manualEntry: !this.state.manualEntry })
              }
            >
            ->
          </Text>
        </Text>

        <Box
          mt="auto"
          width="100%"
          display="flex"
          flexDirection="column"
        >
          <Text color={weatherStyle.text}>{c.summary}</Text>
          <Text color={weatherStyle.text}>{Math.round(c.temperature)}°</Text>
          <Text color={weatherStyle.text}>{nextSolarEvent}</Text>
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
        <Box
          p='2'
          width='100%'
          height='100%'
          backgroundColor='white'
          color='black'
        >
            <Icon icon='Weather' color='black' display='inline' style={{ position: 'relative', top: '.3em' }} />
            <Text>Weather</Text>
          <Text pt='2' width='100%' display='flex' flexDirection='column'>
          Loading, please check again later...
          </Text>
        </Box>
      ));
    }
    return this.renderNoData();
  }
}

