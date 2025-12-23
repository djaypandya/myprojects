'use client';

import styles from './Postcard.module.css';
import Image from 'next/image';
import Link from 'next/link';
import { useRouter } from 'next/navigation';
import { useEffect, useRef } from 'react';

/* "Arrows should feel symbolic or hand-drawn" - SVG Icons */
const ArrowLeft = () => (
    <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.5">
        <path d="M10 19l-7-7m0 0l7-7m-7 7h18" strokeLinecap="round" strokeLinejoin="round" />
    </svg>
); // Placeholder for "Hand drawn" - standard icon for now, ideally would use a custom SVG path with wiggle.

const ArrowRight = () => (
    <svg width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="1.5">
        <path d="M14 5l7 7m0 0l-7 7m7-7H3" strokeLinecap="round" strokeLinejoin="round" />
    </svg>
);

interface PostcardProps {
    imageSrc: string;
    imageAlt: string;
    metadataTop?: string;
    metadataBottom?: string;
    prevLink?: string;
    nextLink?: string;
}

export default function Postcard({
    imageSrc,
    imageAlt,
    metadataTop,
    metadataBottom,
    prevLink,
    nextLink
}: PostcardProps) {
    const router = useRouter();
    const touchStart = useRef<number | null>(null);
    const touchEnd = useRef<number | null>(null);

    // Basic Keyboard Navigation
    useEffect(() => {
        const handleKeyDown = (e: KeyboardEvent) => {
            if (e.key === 'ArrowLeft' && prevLink) {
                router.push(prevLink);
            } else if (e.key === 'ArrowRight' && nextLink) {
                router.push(nextLink);
            }
        };
        window.addEventListener('keydown', handleKeyDown);
        return () => window.removeEventListener('keydown', handleKeyDown);
    }, [prevLink, nextLink, router]);

    // Touch handlers
    const onTouchStart = (e: React.TouchEvent) => {
        touchEnd.current = null;
        touchStart.current = e.targetTouches[0].clientX;
    };

    const onTouchMove = (e: React.TouchEvent) => {
        touchEnd.current = e.targetTouches[0].clientX;
    };

    const onTouchEnd = () => {
        if (!touchStart.current || !touchEnd.current) return;
        const distance = touchStart.current - touchEnd.current;
        const isLeftSwipe = distance > 50;
        const isRightSwipe = distance < -50;

        if (isLeftSwipe && nextLink) {
            router.push(nextLink);
        }
        if (isRightSwipe && prevLink) {
            router.push(prevLink);
        }
    };

    return (
        <div
            className={styles.container}
            onTouchStart={onTouchStart}
            onTouchMove={onTouchMove}
            onTouchEnd={onTouchEnd}
        >
            {prevLink && (
                <Link href={prevLink} className={styles.navArrowLeft} aria-label="Previous Project">
                    <ArrowLeft />
                </Link>
            )}

            <div className={styles.postcardWrapper}>
                {metadataTop && <div className={styles.metadataTop}>{metadataTop}</div>}

                <div className={styles.postcardInner}>
                    <Image
                        src={imageSrc}
                        alt={imageAlt}
                        fill
                        className={styles.image}
                        priority
                    />
                </div>

                {metadataBottom && <div className={styles.metadataBottom}>{metadataBottom}</div>}
            </div>

            {nextLink && (
                <Link href={nextLink} className={styles.navArrowRight} aria-label="Next Project">
                    <ArrowRight />
                </Link>
            )}
        </div>
    );
}
