import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { ContactItem } from '/components/lib/contact-item';
import { ShareSheet } from '/components/lib/share-sheet';
import { Sigil } from '../lib/icons/sigil';

export class ContactSidebar extends Component {
  render() {
    const { props } = this;

    let group = new Set(Array.from(props.group));
    let responsiveClasses =
      props.activeDrawer === "contacts" ? "db" : "dn db-ns";

    let me = (window.ship in props.defaultContacts) ?
      props.defaultContacts[window.ship] : { color: '0x0', nickname: null};
    
    let shareSheet = 
      !(window.ship in props.contacts) ?
      ( <ShareSheet
          ship={window.ship}
          nickname={me.nickname}
          color={me.color}
          path={props.path}
          selected={props.path + "/" + window.ship === props.selectedContact}
        />
      ) : (<div></div>);
    group.delete(window.ship);

    let contactItems = 
      Object.keys(props.contacts)
      .map((contact) => {
        group.delete(contact);
        let path = props.path + "/" + contact;
        let obj = props.contacts[contact];
        return (
          <ContactItem
            key={contact}
            ship={contact}
            nickname={obj.nickname}
            color={obj.color}
            path={props.path}
            selected={path === props.selectedContact}
            share={false}
          />
        );
      });

    let groupItems =
      Array.from(group).map((member) => {
        return (
          <div className="pl4 pt1 pb1 f9 flex justify-start content-center">
            <Sigil ship={member} color="#aaaaaa" size={32} />
            <p className="f9 w-70 dib v-mid ml2 nowrap mono"
              style={{ paddingTop: 6, color: '#aaaaaa' }}>
              ~{member}
            </p>
          </div>
        );
      });

    return (
      <div className={`bn br-m br-l br-xl b--black lh-copy h-100 flex-shrink-0 
      flex-basis-100-s flex-basis-30-ns mw5-m mw5-l mw5-xl relative 
      overflow-hidden ` + responsiveClasses}>
        <div className="pt3 pb6 pl3 f8 db dn-m dn-l dn-xl">
          <Link to="/~contacts/">{"⟵ All Groups"}</Link>
        </div>
        <div className="overflow-y-scroll h-100">
          {shareSheet}
          <h2 className="f9 pt4 pr4 pb2 pl4 gray2 c-default">Members</h2>
          {contactItems}
          {groupItems}
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
    );
  }
}

