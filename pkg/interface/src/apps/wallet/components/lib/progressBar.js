import React from 'react';

// progress is [0, 1]
export default function ProgressBar(progress) {
  return (
    <div className='rel bg-gray3 full' style={{ height: '14px' }}>
      <div
        className="abs bg-green2 animated-width tc mono"
        style={{
          top: 0,
          bottom: 0,
          left: 0,
          width: `${progress * 100.0}%`,
        }}
      >
      {`${(progress * 100.0).toFixed(2)}%`}
      </div>
    </div>
  );
}
