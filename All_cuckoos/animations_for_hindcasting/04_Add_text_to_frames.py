import cv2

# reading text file with text. each line index is frame pos
with open('/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/pdf_titles.txt') as f:
    frames = {k: v for k, v in enumerate(f.readlines())}

cap = cv2.VideoCapture('/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/CuOpt_animation_pb/pngs_pb/hindcast_CuOpt_pb.mp4')
if not cap.isOpened():
    print("Error: Could not open input video file")
    exit()

fps = cap.get(cv2.CAP_PROP_FPS)
w = int(cap.get(cv2.CAP_PROP_FRAME_WIDTH))
h = int(cap.get(cv2.CAP_PROP_FRAME_HEIGHT))

fourcc = cv2.VideoWriter_fourcc(*'mp4v')
out = cv2.VideoWriter('/home/enourani/ownCloud - enourani@ab.mpg.de@owncloud.gwdg.de/Work/Collaborations/cuckoos/Kasper_cuckoos/figs_to_make_animation/animations/hindcast_CuOpt_pb_title.mp4', fourcc, fps, (w, h))
if not out.isOpened():
    print("Error: Could not create output video file")
    exit()

font = cv2.FONT_HERSHEY_DUPLEX  # Change to Arial-like font

frame_count = 0
while cap.isOpened():
    ret, frame = cap.read()
    if ret:
        frame_no = cap.get(cv2.CAP_PROP_POS_FRAMES)
        if frame_no in frames:
            text = frames[frame_no].strip()
            text_size, _ = cv2.getTextSize(text, font, 1, 2)
            text_x = int((w - text_size[0]) // 2)
            text_y = h- 200  # Move text position down
            cv2.putText(frame, text, (text_x, text_y), font, 1, (0, 0, 0), 2, cv2.LINE_AA)
        out.write(frame)
        frame_count += 1
    else:
        break

print(f"Processed {frame_count} frames")

cap.release()
out.release()
cv2.destroyAllWindows()
