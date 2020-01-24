import React, { Component } from 'react'
import { CommentItem } from './comment-item';
import { CommentsPagination } from './comments-pagination';

import { uxToHex } from '../../lib/util'; 
import { api } from '../../api';

export class Comments extends Component {

  componentDidMount() {
    let page = "page" + this.props.commentPage;
    let comments = !!this.props.comments;
    if ((!comments[page]) && (page !== "page0")) {
      api.getCommentsPage(
        this.props.path, 
        this.props.url, 
        this.props.linkPage, 
        this.props.linkIndex, 
        this.props.commentPage);
    }
  }

  componentDidUpdate(prevProps) {
    let page = "page" + this.props.commentPage;
    if (prevProps !== this.props) {
      if (!!this.props.comments) {
        if ((page !== "page0") && (!this.props.comments[page])) {
          api.getCommentsPage(
            this.props.path, 
            this.props.url, 
            this.props.linkPage, 
            this.props.linkIndex, 
            this.props.commentPage);
        }
      }
    }
  }

  render() {
    let props = this.props;
    
    let page = "page" + props.commentPage;

    let commentsObj = !!props.comments
    ? props.comments
    : {};

    let commentsPage = !!commentsObj[page]
    ? commentsObj[page]
    : {};

    let total = !!props.comments
    ? props.comments["total-pages"]
    : {};

    let commentsList = Object.keys(commentsPage)
    .map((entry) => {

      let commentObj = commentsPage[entry]
      let { ship, time, udon } = commentObj;

      let members = !!props.members 
      ? props.members
      : {};

      let nickname = !!members[ship]
      ? members[ship].nickname
      : "";

      let nameClass = nickname ? "inter" : "mono";

      let color = !!members[ship]
      ? uxToHex(members[ship].color)
      : "000000";

      return(
        <CommentItem
          key={time}
          ship={ship}
          time={time}
          content={udon}
          nickname={nickname}
          nameClass={nameClass}
          color={color}
        />
      )
    })
    return (
      <div>
        {commentsList}
        <CommentsPagination
        key={props.path + props.commentPage}
        path={props.path}
        popout={props.popout}
        linkPage={props.linkPage}
        linkIndex={props.linkIndex}
        commentPage={props.commentPage}
        total={total}/>
      </div>
    )
  }
}

export default Comments;