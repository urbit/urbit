import React, { Component } from 'react';
import { CommentItem } from './comment-item';
import { CommentsPagination } from './comments-pagination';

import { getContactDetails } from '../../lib/util';
import { api } from '../../api';

export class Comments extends Component {
  constructor(props) {
    super(props);
    this.state = {};
  }

  componentDidMount() {
    const page = this.props.commentPage;
    if (!this.props.comments ||
        !this.props.comments[page] ||
        this.props.comments.local[page]
    ) {
      this.setState({ requested: this.props.commentPage });
      api.getCommentsPage(
        this.props.resourcePath,
        this.props.url,
        this.props.commentPage);
    }
  }

  render() {
    const props = this.props;

    const page = props.commentPage;

    const commentsObj = props.comments
    ? props.comments
    : {};

    const commentsPage = commentsObj[page]
    ? commentsObj[page]
    : {};

    const total = props.comments
    ? props.comments.totalPages
    : 1;

    const commentsList = Object.keys(commentsPage)
    .map((entry) => {
      const commentObj = commentsPage[entry];
      const { ship, time, udon } = commentObj;

      const contacts = props.contacts
        ? props.contacts
        : {};

      const { nickname, color, member, avatar } = getContactDetails(contacts[ship]);

      const nameClass = nickname ? 'inter' : 'mono';

      return(
        <CommentItem
          key={time}
          ship={ship}
          time={time}
          content={udon}
          nickname={nickname}
          nameClass={nameClass}
          color={color}
          avatar={avatar}
          member={member}
        />
      );
    });
    return (
      <div>
        {commentsList}
        <CommentsPagination
        key={props.resourcePath + props.commentPage}
        resourcePath={props.resourcePath}
        popout={props.popout}
        linkPage={props.linkPage}
        linkIndex={props.linkIndex}
        url={props.url}
        commentPage={props.commentPage}
        total={total}
        />
      </div>
    );
  }
}

export default Comments;
