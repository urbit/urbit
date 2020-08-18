import React, { Component } from 'react';
import { Box, Text } from "@tlon/indigo-react";
import { NotebookItem } from './NotebookItem';

export class GroupItem extends Component {
  render() {
    const { props } = this;
    const association = props.association ? props.association : {};

    let title = association['app-path'] ? association['app-path'] : 'Unmanaged Notebooks';
    if (association.metadata && association.metadata.title) {
      title = association.metadata.title !== ''
        ? association.metadata.title : title;
    }

    const groupedBooks = props.groupedBooks ? props.groupedBooks : [];
    const first = (props.index === 0) ? 'pt1' : 'pt6';

    const notebookItems = groupedBooks.map((each, i) => {
      const unreads = props.notebooks[each]['num-unread'] || 0;
      let title = each.substr(1);
      if (props.notebooks[each].title) {
        title = (props.notebooks[each].title !== '')
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
      );
    });
    return (
      <Box className={first}>
        <Box fontSize={0} px={3} fontWeight="700" pb={2} color="lightGray">{title}</Box>
        {notebookItems}
      </Box>
    );
  }
}

export default GroupItem;
