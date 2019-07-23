import React, { Component } from 'react';
import { subscription } from '/subscription';
import { api } from '/lib/api';
import classnames from 'classnames';
import moment from 'moment';

import Dropdown from '/components/dropdown';

export default class Header extends Component {

  constructor(props) {
    super(props);
    this.interval = null;
    this.timeout = null;
    
    this.state = {
      moment: moment()
    };
  }

  componentDidMount() {
    let sec = parseInt(moment().format("s"), 10);
    
    this.timeout = setTimeout(() => {
      this.setState({
        moment: moment()
      });
      this.interval = setInterval(() => {
        this.setState({
          moment: moment()
        });
      }, 60000);
    }, (60 - sec) * 1000);
  }
  
  componentWillUnmount() {
    clearTimeout(this.timeout);
    clearInterval(this.interval);
  }

  render() {
    return (
      <header className="w-100 h2 cf">
        <div className="fl h2 bg-black">
        </div>
        <div className="fr h2 bg-black">
          <p className="white v-mid h2 sans-serif dtc pr2">{this.state.moment.format("MMM DD")}</p>
          <p className="white v-mid h2 sans-serif dtc pr2">{this.state.moment.format("hh:mm a")}</p>
        </div>
      </header>
    );
  }

}

