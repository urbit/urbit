import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { ContactItem } from '/components/lib/contact-item';
import { ShareSheet } from '/components/lib/share-sheet';
import { Sigil } from '../lib/icons/sigil';
import { Spinner } from '../lib/icons/icon-spinner';
import { cite } from '../../lib/util';

export class ContactSidebar extends Component {
  constructor(props) {
    super(props);
    this.state = {
      awaiting: false
    }
  }
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

    let adminOpt = (props.path.includes(`~${window.ship}/`))
      ? "dib" : "dn";

    let groupItems =
      Array.from(group).map((member) => {
        return (
          <div
          key={member}
          className={"pl4 pt1 pb1 f9 flex justify-start content-center " +
            "bg-white bg-gray0-d relative"}>
            <Sigil
              ship={member}
              color="#000000"
              size={32}
              classes="mix-blend-diff"
              />
            <p className="f9 w-70 dib v-mid ml2 nowrap mono truncate"
              style={{ paddingTop: 6, color: '#aaaaaa' }}
              title={member}>
              {cite(member)}
            </p>
            <p className={"v-mid f9 mh3 red2 pointer " + adminOpt}
              style={{paddingTop: 6}}
              onClick={() => {
                this.setState({awaiting: true}, (() => {
                  props.api.groupRemove(props.path, [`~${member}`])
                    .then(() => {
                      this.setState({awaiting: false})
                    })
                }))
              }}>
              Remove
            </p>
          </div>
        );
      });

    let detailHref = `/~groups/detail${props.path}`

    return (
      <div className={"bn br-m br-l br-xl b--gray4 b--gray1-d lh-copy h-100 " +
      "flex-basis-100-s flex-basis-30-ns mw5-m mw5-l mw5-xl relative " +
      "overflow-hidden flex-shrink-0 " + responsiveClasses}>
        <div className="pt3 pb5 pl3 f8 db dn-m dn-l dn-xl">
          <Link to="/~groups/">{"⟵ All Groups"}</Link>
        </div>
        <div className="overflow-auto h-100">
          <Link
            to={"/~groups/add" + props.path}
            className={((props.path.includes(window.ship))
              ? "dib"
              : "dn")}>
            <p className="f9 pl4 pt0 pt4-m pt4-l pt4-xl green2 bn">Add to Group</p>
          </Link>
          <Link to={detailHref}
            className="dib dn-m dn-l dn-xl f9 pl4 pt0 pt4-m pt4-l pt4-xl gray2 bn">Channels</Link>
          {shareSheet}
          <h2 className="f9 pt4 pr4 pb2 pl4 gray2 c-default">Members</h2>
          {contactItems}
          {groupItems}
        </div>
        <Spinner awaiting={this.state.awaiting} text="Removing from group..." classes="pa2 ba absolute right-1 bottom-1 b--gray1-d" />
        </div>
    );
  }
}

