import React, { Component } from 'react';
import classnames from 'classnames';
import moment from 'moment';

class IconWithData extends Component {
  render() {
    const { props } = this;

    return (
      <div className='mt2'>
        <p className="f9 white">{props.text}</p>
      </div>
    );
  }
}

export default class WeatherTile extends Component {

  constructor(props) {
    super(props);

    let ship = window.ship;
    let api = window.api;

    this.state = {
      latlng: '',
      manualEntry: false,
      error: false
    };
  }

  locationSubmit() {
    navigator.geolocation.getCurrentPosition((res) => {
      let latlng = `${res.coords.latitude},${res.coords.longitude}`;
      this.setState({
        latlng
      }, (err) => {
        console.log(err);
      }, { maximumAge: Infinity, timeout: 10000 });
      api.action("clock", "json", latlng);
      api.action('weather', 'json', latlng);
    });
  }

  manualLocationSubmit() {
    event.preventDefault();
    let gpsInput = document.getElementById('gps');
    let latlngNoSpace = gpsInput.value.replace(/\s+/g, '');
    let latlngParse = /-?[0-9]+(?:\.[0-9]*)?,-?[0-9]+(?:\.[0-9]*)?/g;
    if (latlngParse.test(latlngNoSpace)) {
      let latlng = latlngNoSpace;
      this.setState({latlng}, (err) => {console.log(err)}, {maximumAge: Infinity, timeout: 10000});
      api.action("clock", "json", latlng);
      api.action('weather', 'json', latlng);
      this.setState({manualEntry: !this.state.manualEntry});
    }
    else {
      this.setState({error: true});
      return false;
    }
  }

  keyPress(e) {
    if (e.keyCode === 13) {
      e.preventDefault();
      this.manualLocationSubmit(e.target.value);
    }
  }

  renderWrapper(child) {
    return (
      <div
        className="pa2 relative bg-white b--black ba"
        style={{
          width: 126,
          height: 126,
        }}>
        {child}
      </div>
    );
  }

  renderManualEntry() {
    let secureCheck;
    let error;
    if (this.state.error === true) {
      error = <p
          className="f9 red2 pt1">
          Incorrect latitude/longitude formatting. Please try again. <br/>
          (eg. "29.558107, -95.089023")
        </p>
    }
    if (location.protocol === "https:") {
      secureCheck = <a
        className="black f9 absolute pointer"
        style={{right: 8, top: 8}}
        onClick={() => this.locationSubmit()}>Detect -></a>
    }
    return this.renderWrapper(
      <div>
        <a
          className="f9 black pointer"
          onClick={() =>
            this.setState({ manualEntry: !this.state.manualEntry })
          }>
          &lt;&#45;
        </a>
        {secureCheck}
        <p className="f9 black pt2">
          Please enter your{" "}
          <a
            className="white"
            href="https://latitudeandlongitude.org/"
            target="_blank">
            latitude and longitude
          </a>
          .
        </p>
        {error}
        <div className="flex">
          <form className="flex absolute" style={{ bottom: 0, left: 8 }}>
            <input
              id="gps"
              className="w-100 black pa1 bg-transparent outline-0 bn bb-ns b--black f9"
              type="text"
              placeholder="29.558107, -95.089023"
              onKeyDown={this.keyPress.bind(this)}></input>
            <input
              className="bg-transparent black outliner-0 bn pointer f9 flex-shrink-0"
              type="submit"
              onClick={() => this.manualLocationSubmit()}
              value="->"></input>
          </form>
        </div>
      </div>
    );
  }

  renderNoData() {
    return this.renderWrapper((
      <div onClick={() => this.setState({manualEntry: !this.state.manualEntry})}>
          <p className="black f9 absolute"
            style={{left: 8, top: 8}}>
            Weather
          </p>
        <p className="absolute w-100 flex-col f9 black" style={{verticalAlign: "bottom", bottom: 8, left: 8, cursor: "pointer"}}>-> Set location</p>
      </div>
    ));
  }

  renderWithData(data) {
    let c = data.currently;
    let d = data.daily.data[0];

    let da = moment.unix(d.sunsetTime).format('h:mm a') || '';

    return this.renderWrapper(
      <div>
        <p className="black f9 absolute" style={{ left: 8, top: 8 }}>
          Weather
        </p>
        <a
          className="black f9 absolute pointer"
          style={{ right: 8, top: 8 }}
          onClick={() =>
            this.setState({ manualEntry: !this.state.manualEntry })
          }>
          ->
        </a>
        <div className="w-100 absolute" style={{ left: 8, bottom: 8 }}>
          <p className="f9 black">{c.summary}</p>
          <p className="f9 pt1 black">{Math.round(c.temperature)}°</p>
          <p className="f9 pt1 black">Sunset at {da}</p>
        </div>
      </div>
    );
  }

  render() {
    let data = !!this.props.data ? this.props.data : {};

    if (this.state.manualEntry === true) {
      return this.renderManualEntry();
    }

    if ('currently' in data && 'daily' in data) {
      return this.renderWithData(data);
    }

    return this.renderNoData();
  }

}

window.weatherTile = WeatherTile;
