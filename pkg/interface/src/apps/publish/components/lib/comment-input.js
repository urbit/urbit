import React from 'react';

const CommentInput = React.forwardRef((props, ref) => (
  <textarea
    {...props}
    ref={ref}
    id="comment"
    name="comment"
    placeholder="Leave a comment here"
    className={
      'f9 db border-box w-100 ba b--gray3 pt2 ph2 br1 ' +
      'b--gray2-d mb2 focus-b--black focus-b--white-d white-d bg-gray0-d'
    }
    aria-describedby="comment-desc"
    style={{ height: '4rem', resize: 'vertical' }}
    onKeyDown={(e) => {
      if (
        (e.getModifierState('Control') || event.metaKey) &&
        e.key === 'Enter'
      ) {
        props.onSubmit();
      }
    }}
  ></textarea>
));

CommentInput.displayName = 'commentInput';

export default CommentInput;
