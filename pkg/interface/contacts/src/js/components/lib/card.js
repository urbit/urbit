import React, { Component } from 'react';
import { Sigil } from './icons/sigil';

import { Route, Link } from 'react-router-dom';

export class ContactCard extends Component {
    render() {
      //TODO "Share card" if it's /me -> sends to /~/default of recipient
        return (
            <div className="h-100 w-100 overflow-x-hidden">
            <div className="w-100 h2 dn-m dn-l dn-xl inter pb6 pl3 pt3 f8">
              <Link to="/~contacts/">{"⟵"}</Link>
            </div>
            <div className="w-100 bb b--gray4">
              <button className="ml3 mt2 mb2 f9 pa2 ba br3 b--black">Edit Contact Info</button>
            </div>
            <div className="w-100 flex justify-center pa4 pa0-xl pt4-xl">
              <div className="w-100 mw6 tc">
              {/*TODO default to sigil, but show avatar if exists for contact */}
                <Sigil ship={this.props.ship} size={128} color={this.props.contact.color}/>
                <div className="w-100 pt8 lh-copy tl">
                  <p className="f9 gray2">Ship Name</p>
                  <p className="f8 mono">~{this.props.ship}</p>
                </div>
              </div>
            </div>
            </div>
        )
    }
}

export default ContactCard
