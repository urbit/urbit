import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
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
          nickname={obj.nickname}
          color={obj.color}
          path={path}
          selected={selected}
          />
        )
      })
      //TODO if your contact in this group is completely empty, show prompt to "share details selectively", using your root identity as template
      // (will require its own route, see root.js)
        return (
            <div className={`bn br-m br-l br-xl b--black lh-copy h-100 flex-shrink-0 
            flex-basis-100-s flex-basis-30-ns mw5-m mw5-l mw5-xl relative 
            overflow-hidden ` + responsiveClasses}>
              <div className="pt3 pb6 pl3 f8 db dn-m dn-l dn-xl">
                <Link to="/~contacts/">{"⟵ All Groups"}</Link>
              </div>
              <div className="overflow-y-scroll h-100">
                <h2 className="f9 pt4 pr4 pb2 pl4 gray2 c-default">Members</h2>
                {contactItems}
              </div>
              <div
                className={"bg-white z2 bt b--gray4 absolute w-100 " +
                ((this.props.path.includes(window.ship))
                  ? "dt"
                  : "dn")}
                style={{ bottom: 0, height: 48 }}>
                <Link to={"/~contacts/add" + props.path} className="dtc v-mid">
                  <p className="f9 pl4 black bn">Add New Member to Group</p>
                </Link>
              </div>
            </div>
        )
    }
}

export default Contacts;
