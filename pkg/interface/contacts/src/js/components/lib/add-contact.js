import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';

export class AddScreen extends Component {
  render() {
    const { props } = this; 
    return (
      <div className="h-100 w-100 flex flex-column overflow-y-scroll">
        <div className="w-100 dn-m dn-l dn-xl inter pt1 pb6 pl3 pt3 f8">
          <Link to={"/~contacts" + props.path}>{"⟵ All Contacts"}</Link>
        </div>
      </div>
    )
  }
}

export default AddScreen;
