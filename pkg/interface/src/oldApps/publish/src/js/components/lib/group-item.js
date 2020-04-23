import React, { Component } from 'react';
import { NotebookItem } from './notebook-item';

export class GroupItem extends Component {
  render() {
    const { props, state } = this;
    let association = !!props.association ? props.association : {};

    let title = association["app-path"] ? association["app-path"] : "Unmanaged Notebooks";
    if (association.metadata && association.metadata.title) {
      title = association.metadata.title !== ""
        ? association.metadata.title : title;
    }

    let groupedBooks = !!props.groupedBooks ? props.groupedBooks : [];
    let first = (props.index === 0) ? "pt1" : "pt4";

    let notebookItems = groupedBooks.map((each, i) => {
      let unreads = props.notebooks[each]["num-unread"] || 0;
      let title = each.substr(1);
      if (props.notebooks[each].title) {
        title = (props.notebooks[each].title !== "")
          ? props.notebooks[each].title : title;
      }
      return (
        <NotebookItem
          key={i}
          unreadCount={unreads}
          title={title}
          path={each}
          selected={(props.path === each)}
        />
      )
    })
    return (
      <div className={first}>
      <p className="f9 ph4 pb2 fw6 gray3">{title}</p>
      {notebookItems}
      </div>
    )
  }
}

export default GroupItem;