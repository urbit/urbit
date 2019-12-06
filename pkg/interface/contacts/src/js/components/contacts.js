import React, { Component } from 'react';

import { ContactItem } from './lib/contact-item';

export class Contacts extends Component {
    render() {
      const { props } = this;

      let contactProp = ((props.contacts === undefined) || (props.contacts === null))
      ? {}
      : props.contacts;

      let responsiveClasses =
        this.props.activeDrawer === "contacts" ? "db" : "dn db-ns";

      let contactItems = (Object.keys(contactProp))
      .map((contact) => {
        let path = props.path + "/" + contact;
        let obj = props.contacts[contact];
        let selected = (path === props.selectedContact);
        return(
          <ContactItem
          key={contact}
          ship={contact}
          color={obj.color}
          path={path}
          selected={selected}
          />
        )
      })
        return (
            <div className={`br b--black lh-copy h-100 flex-shrink-0 
            flex-basis-100-s flex-basis-300-ns relative ` + responsiveClasses}>
            <h2 className="f9 pt4 pr4 pb2 pl4 gray2 c-default">Members</h2>
            {contactItems}
            </div>
        )
    }
}

export default Contacts;
