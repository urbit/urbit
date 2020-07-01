import React, { Component } from 'react';

export class MessageScreen extends Component {
  render() {
    return (
      <div className="h-100 w-100 overflow-x-hidden flex flex-column bg-white bg-gray0-d dn db-ns">
        <div className="pl3 pr3 pt2 dt pb3 w-100 h-100">
          <p className="f8 pt3 gray2 w-100 h-100 dtc v-mid tc">
            {this.props.text}
          </p>
        </div>
      </div>
    );
  }
}
