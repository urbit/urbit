import React, { Component } from 'react';
import { Route, Link } from 'react-router-dom';
import { base64urlEncode } from '../../lib/util';

export class CommentsPagination extends Component {
  render() {
    let props = this.props;

    let prevPage = "/" + (Number(props.commentPage) - 1);
    let nextPage = "/" + (Number(props.commentPage) + 1);

    let prevDisplay = ((Number(props.commentPage) > 0))
    ? "dib"
    : "dn";

    let nextDisplay = ((Number(props.commentPage) + 1) < Number(props.total))
    ? "dib"
    : "dn";

    let encodedUrl = base64urlEncode(props.url);
    let popout = (props.popout) ? "/popout" : "";

    return (
      <div className="w-100 relative pt4 pb6">
        <Link
        className={"pb6 absolute inter f8 left-0 " + prevDisplay}
        to={"/~link"
        + popout
        + props.groupPath
        + "/" + props.linkPage
        + "/" + props.linkIndex
        + "/" + encodedUrl
        + "/comments" + prevPage}>
          &#60;- Previous Page
        </Link>
        <Link
        className={"pb6 absolute inter f8 right-0 " + nextDisplay}
        to={"/~link" 
        + popout
        + props.groupPath
        + "/" + props.linkPage 
        + "/" + props.linkIndex 
        + "/" + encodedUrl
        + "/comments" + nextPage}>
          Next Page ->
        </Link>
      </div>
    )
  }
}

export default CommentsPagination;