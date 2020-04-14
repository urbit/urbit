import React from "react";

export const CommentInput = React.forwardRef((props, ref) => (
  <textarea
    {...props}
    ref={ref}
    style={{ resize: "vertical" }}
    id="comment"
    name="comment"
    placeholder="Leave a comment here"
    className={
      "f9 db border-box w-100 ba b--gray3 pt3 ph3 br1 " +
      "b--gray2-d mb2 focus-b--black focus-b--white-d white-d bg-gray0-d"
    }
    aria-describedby="comment-desc"
    style={{ height: "4rem" }}
    onKeyDown={e => {
      if (
        (e.getModifierState("Control") || event.metaKey) &&
        e.key === "Enter"
      ) {
        props.onSubmit();
      }
    }}
  ></textarea>
));
