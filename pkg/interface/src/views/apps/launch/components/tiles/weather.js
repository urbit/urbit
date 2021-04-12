import React from 'react';
import moment from 'moment';
import { Box, Icon, Text, BaseAnchor, BaseInput } from '@tlon/indigo-react';
import ErrorBoundary from '~/views/components/ErrorBoundary';
import withState from '~/logic/lib/withState';
import useLaunchState from '~/logic/state/launch';

import Tile from './tile';

export const weatherStyleMap = {
  Clear: 'rgba(67, 169, 255, 0.4)',
  Sunny: 'rgba(67, 169, 255, 0.4)',
  PartlyCloudy: 'rgba(178, 211, 255, 0.33)',
  Cloudy: 'rgba(136, 153, 176, 0.43)',
  VeryCloudy: 'rgba(78, 90, 106, 0.43)',
  Fog: 'rgba(100, 119, 128, 0.12)',
  LightShowers: 'rgba(121, 148, 185, 0.33)',
  LightSleetShowers: 'rgba(114, 130, 153, 0.33)',
  LightSleet: 'rgba(155, 164, 177, 0.33)',
  ThunderyShowers: 'rgba(53, 77, 103, 0.33)',
  LightSnow: 'rgba(179, 182, 200, 0.33)',
  HeavySnow: 'rgba(179, 182, 200, 0.33)',
  LightRain: 'rgba(58, 79, 107, 0.33)',
  HeavyShowers: 'rgba(36, 54, 77, 0.33)',
  HeavyRain: 'rgba(5, 9, 13, 0.39)',
  LightSnowShowers: 'rgba(174, 184, 198, 0.33)',
  HeavySnowShowers: 'rgba(55, 74, 107, 0.33)',
  ThunderyHeavyRain: 'rgba(45, 56, 66, 0.61)',
  ThunderySnowShowers: 'rgba(40, 54, 79, 0.46)',
  default: 'transparent'
};

const imperialCountries = [
  'United States of America',
  'Myanmar',
  'Liberia',
];

class WeatherTile extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      location: '',
      manualEntry: false,
      error: false
    };
  }

  // geolocation and manual input functions
  locationSubmit() {
    navigator.geolocation.getCurrentPosition((res) => {
      const location = `${res.coords.latitude},${res.coords.longitude}`;
      this.setState({
        location
      }, (err) => {
        console.log(err);
      }, { maximumAge: Infinity, timeout: 10000 });
      this.props.api.launch.weather(location);
      this.setState({ manualEntry: !this.state.manualEntry });
    });
  }

  manualLocationSubmit(event) {
    event.preventDefault();
    const location = document.getElementById('location').value;
    this.setState({ location }, (err) => {
      console.log(err);
      }, { maximumAge: Infinity, timeout: 10000 });
      this.props.api.launch.weather(location);
      this.setState({ manualEntry: !this.state.manualEntry });
  }

  // set appearance based on weather
  colorFromCondition(data) {
    let weatherDesc = data['current-condition'][0].weatherDesc[0].value;
    return weatherStyleMap[weatherDesc] || weatherStyleMap.default;
  }

  // all tile views
  renderWrapper(child, backgroundColor = 'white') {
    return (
      <ErrorBoundary>
        <Tile bg='white' backgroundColor={backgroundColor}>
          {child}
        </Tile>
      </ErrorBoundary>
    );
  }

  renderManualEntry(data) {
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
    let locationName;
    if ('nearest-area' in data) {
      locationName = data['nearest-area'][0].areaName[0].value;
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
          Please enter your location.
          {locationName ? ` Current location is near ${locationName}.` : ''}
        </Text>
        {error}
        <Box mt='auto' display='flex' marginBlockEnd='0'>
          <BaseInput
            id="location"
            size="10"
            width='100%'
            color='black'
            fontSize='0'
            backgroundColor='transparent'
            border='0'
            type="text"
            autoFocus
            placeholder="GPS, ZIP, City"
            onKeyDown={(e) => {
              if (e.key === 'Enter') {
                this.manualLocationSubmit(e);
              }
            }}
          />
          <BaseInput
            backgroundColor='transparent'
            color='black'
            cursor='pointer'
            flexShrink='0'
            pl='1'
            fontSize='0'
            border='0'
            type="submit"
            onClick={this.manualLocationSubmit.bind(this)}
            value="->"
          />
        </Box>
      </Box>
    );
  }

  renderNoData() {
    return this.renderWrapper(
      <Box
        display='flex'
        flexDirection='column'
        justifyContent='space-between'
        height='100%'
        onClick={() => this.setState({ manualEntry: !this.state.manualEntry })}
      >
        <Box>
          <Icon icon='Weather' display='inline-block' verticalAlign='top' mt='3px' mr='2' />
          <Text>Weather</Text>
        </Box>
        <Text style={{ cursor: 'pointer' }}>
          -> Set location
        </Text>
      </Box>
    );
  }

  renderWithData(data) {
    const locationName = data['nearest-area'][0].areaName[0].value;
    const c = data['current-condition'][0];
    const d = data['weather'][0];
    const bg = this.colorFromCondition(data);

    const sunset = moment(moment().format('YYYY-MM-DD') + ' '  + d.astronomy[0].sunset, 'YYYY-MM-DD hh:mm A');
    const sunsetDiff = sunset.diff(moment(), 'hours');

    const sunrise = moment(moment().format('YYYY-MM-DD') + ' '  + d.astronomy[0].sunrise, 'YYYY-MM-DD hh:mm A');
    let sunriseDiff = sunrise.diff(moment(), 'hours');

    if (sunriseDiff > 24) {
      sunriseDiff = sunriseDiff - 24;
    } else if (sunriseDiff < 0) {
      sunriseDiff = sunriseDiff + 24;
    }

    const nextSolarEvent = sunsetDiff > 0
      ? `Sun sets in ${sunsetDiff}h`
      : `Sun rises in ${sunriseDiff}h`;

    const temp = data['nearest-area'] && imperialCountries.includes(data['nearest-area'][0].country[0].value)
      ? `${Math.round(c.temp_F)}℉`
      : `${Math.round(c.temp_C)}℃`;

    return this.renderWrapper(
      <Box
        width='100%'
        height='100%'
        display='flex'
        flexDirection='column'
        alignItems='space-between'
        title={`${locationName} Weather`}
      >
        <Text>
          <Icon icon='Weather' display='inline' mr='2' style={{ position: 'relative', top: '.3em' }} />
            <Text
              cursor='pointer'
              onClick={() =>
                this.setState({ manualEntry: !this.state.manualEntry })
              }
            >
            Weather ->
          </Text>
        </Text>

        <Box
          mt="auto"
          width="100%"
          display="flex"
          flexDirection="column"
        >
          <Text>{c.weatherDesc[0].value.replace(/([a-z])([A-Z])/g, '$1 $2')}</Text>
          <Text>{temp}</Text>
          <Text>{nextSolarEvent}</Text>
        </Box>
      </Box>, bg);
  }

  render() {
    const data = this.props.weather ? this.props.weather : {};

    if (this.state.manualEntry === true) {
      return this.renderManualEntry(data);
    }

    if ('currently' in data) { // Old weather source
      this.props.api.launch.weather(this.props.location);
    }

    if ('current-condition' in data && 'weather' in data) {
      return this.renderWithData(data);
    }

    if (this.props.location) {
      return this.renderWrapper(
        <Box
          width='100%'
          height='100%'
          backgroundColor='white'
          color='black'
          display="flex"
          flexDirection="column"
          justifyContent="flex-start"
        >
          <Text><Icon icon='Weather' color='black' display='inline' mr='2' style={{ position: 'relative', top: '.3em' }} /> Weather</Text>
          <Text width='100%' display='flex' flexDirection='column' mt={1}>
            Loading, please check again later...
          </Text>
          <Text mt="auto">
            Set new location{' '}
            <Text
                cursor='pointer'
                onClick={() =>
                  this.setState({ manualEntry: !this.state.manualEntry })
                }
              >
              ->
            </Text>
          </Text>
        </Box>
      );
    }
    return this.renderNoData();
  }
}

export default withState(WeatherTile, [[useLaunchState]]);
