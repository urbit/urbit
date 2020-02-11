import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';

export class NotebookItem extends Component {
  render() {
    let { props } = this;

    let selectedClass = (props.selected) ? "bg-gray5 b--gray4" : "b--gray4";

    let postCount = (props.total === 1)
      ? `${props.total} post` : `${props.total} posts`;

    let unread = (props.unreadCount > 0)
      ? `${props.unreadCount} unread` : "";

    let notebookContacts = (props.contactsPath in props.contacts)
      ? props.contacts[props.contactsPath] : {};
    let contact = !!(props.author.substr(1) in notebookContacts)
      ? notebookContacts[props.author.substr(1)] : false;

    let name = props.author;
    if (contact) {
      name = (contact.nickname.length > 0)
        ? contact.nickname : props.author;
    }

    return (
      <Link
      to={"/~publish/notebook/" + props.path}>
        <div className={"w-100 v-mid f9 pl4 bb " + selectedClass}>
          <p className="f9 pt1">{props.title}</p>
          <p className="f9 gray2">by
            <span className={"pl1 " + ((name === props.author) ? "mono" : "")}>
              {name}
            </span>
          </p>
          <p className="f9 pb1">
          {postCount}
            <span className="green2 ml3">
              {unread}
            </span>
          </p>
        </div>
      </Link>
    );
  }
}

export default NotebookItem
