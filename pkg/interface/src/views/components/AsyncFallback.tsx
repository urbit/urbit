import React from 'react';

interface AsyncFallbackProps {
  fallback?: JSX.Element;
}
class AsyncFallback extends React.Component<
  AsyncFallbackProps,
  {
    error: boolean;
  }
> {
  constructor(props: AsyncFallbackProps) {
    super(props);
    this.state = { error: false };
  }

  static getDerivedStateFromError(error) {
    return { error: true };
  }

  componentDidCatch(error, info) {}

  render() {
    const { fallback, children } = this.props;
    return (
      <React.Suspense fallback={fallback}>
        {this.state.error ? fallback : children}
      </React.Suspense>
    );
  }
}

export default AsyncFallback;
