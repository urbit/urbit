import React, { Component } from 'react';
import { Sigil } from './icons/sigil';

export class ContactCard extends Component {
    render() {
        return (
            <div className="h-100 w-100 flex justify-center overflow-x-hidden">
              <div className="w-50-xl flex justify-center pt4">
                <Sigil ship={window.ship} size={128} color={this.props.contact.color}/>
              </div>
            </div>
        )
    }
}

export default ContactCard
